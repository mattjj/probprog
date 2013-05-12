(declare (usual-integrations))
(load "util")
(load "randutil")
(load "exact-matrices")

(define (gaussian-rv? rv)
  (and (random-value? rv)
       (eq? (random-value:type rv) 'gaussian)))

(define (gaussian mean var)
  (let ((var (random-value:force var)))
    (if (gaussian-rv? mean)
      (gaussian:+ mean (gaussian 0 var))
      (gaussian-builder:new-independent (random-value:force mean) var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generics ops to plug in ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gaussian:+ . args)
  ((partition! gaussian-rv? args)
   (lambda (gaussians nongaussians)
     (gaussian-builder:new-sum gaussians (apply default:+ (map random-value:force nongaussians))))))

;; TODO gaussian:*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; joint builder data and operations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO needs to be captured in a dynamic context. macro?
(define *cov-factor* '())
(define *mean-list* '())
(define *sources-list* '())
(define *indices* (make-eq-hash-table))

(define (gaussian-builder:new-independent mean var)
  (if (not (and (number? mean) (number? var)))
    (error "gaussian-builder:new-independent needs number arguments"))
  (let* ((params (gaussian:make-params mean var))
         (rv (random-value:new 'gaussian (gaussian:rvs params))))
    (set! *sources-list* (cons rv *sources-list*))
    (let ((index (gaussian-builder:get-index rv)))
      (gaussian-builder:add-cov-row! (vector (cons index (sqrt var))))
      (gaussian-builder:add-mean! mean)
      rv)))

(define (gaussian-builder:new-sum list-of-gaussians const)
  (define (sum-squares sparse-row-vec)
    (apply default:+ (map square (vector->list (vector-map cdr sparse-row-vec)))))
  (define (sum-of-means gaussian-objects)
    (apply default+ (map gaussian:mean (map gaussian-sample:params gaussian-objects))))

  (let ((newrow (gaussian-builder:row-dot-against-cov-factor list-of-gaussians)))
    (let ((newvar (sum-of-squares newrow))
          (newmean (sum-of-means list-of-gaussians)))
      (let* ((rv (random-value:new 'gaussian (gaussian:rvs (gaussian:make-params newmean newvar))))
             (index (gaussian-builder:get-index rv)))
        (gaussian-builder:add-cov-row! newrow)
        (gaussian-builder:add-mean! newmean)
        rv))))

;; TODO new scaling

;; internals

(define (gaussian-builder:get-index rv)
  (hash-table/lookup *indices* rv
                     (lambda (idx) idx)
                     (lambda () (let ((new-index (hash-table/count *indices*)))
                                  (hash-table/put! *indices* rv new-index)
                                  new-index))))

(define (gaussian-builder:add-cov-row! row)
  (set! *cov-factor* (cons row *cov-factor*)))

(define (gaussian-builder:add-mean! mean)
  (set! *mean-list* (cons mean *mean-list*)))

(define (gaussian-builder:row-dot list-of-gaussians)
  (let ((result-hash (make-eq-hash-table)))
    (let lp1 ((lst list-of-gaussians))
      (if (not (null? lst))
        ;; TODO get rows all at once to save dereferences
        (let* ((row (list-ref *cov-factor* (gaussian-builder:get-index (car lst))))
               (len (vector-length row)))
          (let lp2 ((idx 0))
            (if (< idx len)
              (let* ((pair (vector-ref row idx))
                     (col (car pair))
                     (val (cdr pair)))
                (hash-table/lookup result-hash
                                   col
                                   (lambda (oldval) (hash-table/put! result-hash col (+ val oldval)))
                                   (lambda () (hash-table/put! result-hash col val)))
                (lp2 (fix:+ idx 1)))))
          (lp1 (cdr lst)))))
    (list->vector (hash-table->alist result-hash))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; inference interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define *post-indices* (make-eq-hash-table))

(define (gaussian-builder:get-conditioning-blocks func)
  ((partition! (lambda (pair) (sample:sampled? (car pair)))
               (hash-table->alist *indices*))
   (lambda (sampled unsampled)
     ((partition! (lambda (pair) (memq (car pair) *sources-list*))
                  unsampled)
      (lambda (unsampled-sources unsampled-derived)

        (define (get-dense-block indices)
          (let ((result (m:zeros (length indices) (length *sources-list*))))
            (let ((rows (map (lambda (i) (vector-ref *cov-factor* i))
                             indices))
                  (i 0))
              (for-each (lambda (row)
                          (vector-map (lambda (colval)
                                        (matrix-set! result i (car colval) (cdr colval)))
                                      row)
                          (set! i (fix:+ i 1)))
                        rows))
            result))

        (define (get-dense-col func sample-objs)
          (apply col-matrix (map func sample-objs)))

        (define (get-mean sample-obj)
          (gaussian:mean (gaussian-sample:params sample-obj)))

        (define (get-val sample-obj)
          (gausisan-sample:val sample-obj))

        (let ((sources-factor (get-dense-block (map cdr unsampled-sources)))
              (cond-factor (get-dense-block (map cdr sampled)))
              (derived-factor (get-dense-block (map cdr unsampled-derived)))

              (sources-mean (get-dense-col get-mean (map car unsampled-sources)))
              (cond-mean (get-dense-col get-mean (map car sampled)))
              (derived-mean (get-dense-col get-mean (map car unsampled-derived)))

              (cond-obs (get-dense-col get-val (map car sampled))))
          (func sources-factor sources-mean cond-factor cond-mean cond-obs derived-factor derived-mean)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rollback interface for MH controller ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO

