(declare (usual-integrations))
(load "util")
(load "randutil")
(load "exact-matrices")
(load "pp-records")

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
     (gaussian-builder:new-sum gaussians (apply + (map random-value:force nongaussians))))))

;; TODO gaussian:*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; joint builder data and operations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gaussian-builder:new-independent mean var)
  (if (not (and (number? mean) (number? var)))
    (error "gaussian-builder:new-independent needs numerical arguments"))
  (let* ((params (gaussian:make-params mean var))
         (rv (random-value:new 'gaussian (gaussian:rvs params)))
         (index (gaussian-builder:get-index rv)))
      (gaussian-builder:add-gaussian! mean (vector (cons index (sqrt var))))
      (gaussian-builder:add-source! rv)
      rv))

(define (gaussian-builder:new-sum list-of-gaussians const)
  (define (sum-of-squares sparse-row-vec)
    (vector-sum (vector-map (lambda (pair) (square (cdr pair))) sparse-row-vec)))
  (define (sum-of-means gaussian-objects)
    (vector-sum (gaussian-builder:get-means-of list-of-gaussians)))

  (let ((newrow (gaussian-builder:row-dot-against-cov-factor list-of-gaussians)))
    (let ((newvar (sum-of-squares newrow))
          (newmean (sum-of-means list-of-gaussians)))
      (let* ((rv (random-value:new 'gaussian (gaussian:rvs (gaussian:make-params newmean newvar))))
             (index (gaussian-builder:get-index rv)))
        (gaussian-builder:add-gaussian! newmean newrow)
        rv))))

;; internals

;; TODO needs to be captured in a dynamic context. macro?
(define *cov-factor* (make-vector 16))
(define *means* (make-vector 16))
(define *num-gaussians* 0)
(define *sources-list* '())
(define *indices* (make-eq-hash-table))

(define (gaussian-builder:get-index rv)
  (hash-table/lookup
    *indices* rv
    (lambda (idx) idx)
    (lambda () (let ((new-index (hash-table/count *indices*)))
                 (hash-table/put! *indices* rv new-index)
                 new-index))))

(define (gaussian-builder:get-means-of objects)
  (vector-refs *means* (map gaussian-builder:get-index objects)))

(define (gaussian-builder:add-source! rv)
  (set! *sources-list* (cons rv *sources-list*)))

(define (gaussian-builder:add-gaussian! mean cov-factor-row)
  (let ((len (vector-length *cov-factor*)))
    (if (fix:= *num-gaussians* len)
      (begin
        (set! *cov-factor* (vector-grow *cov-factor* (fix:* 2 len)))
        (set! *means* (vector-grow *means* (fix:* 2 len))))))
  (vector-set! *cov-factor* *num-gaussians* cov-factor-row)
  (vector-set! *means* *num-gaussians* mean)
  (set! *num-gaussians* (fix:+ *num-gaussians* 1)))

(define (gaussian-builder:row-dot-against-cov-factor list-of-gaussians)
  (let ((result-hash (make-eq-hash-table))
        (rows (vector-refs *cov-factor* (map gaussian-builder:get-index list-of-gaussians))))
    (let ((num-rows (vector-length rows)))
      (let lp1 ((row-index 0))
        (if (fix:< row-index num-rows)
          (let* ((row (vector-ref rows row-index))
                 (len (vector-length row)))
            (let lp2 ((col-index 0))
              (if (fix:< col-index len)
                (let* ((pair (vector-ref row col-index))
                       (col (car pair))
                       (val (cdr pair)))
                  (hash-table/lookup result-hash
                                     col
                                     (lambda (oldval) (hash-table/put! result-hash col (+ val oldval)))
                                     (lambda () (hash-table/put! result-hash col val)))
                  (lp2 (fix:+ col-index 1)))))
            (lp1 (fix:+ row-index 1))))))
    (list->vector (hash-table->alist result-hash))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; inference interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define *post-indices* #f)

;; interface for user code

;; TODO this should be a handler for a generic rv:mean
(define (gaussian:posterior-mean rv)
  (error "must be called after gaussian:construct-joint-sample!"))

;; interface for MH controller

;; TODO register this function against type tag
(define (gaussian:construct-joint-sample!)
  (if (not *post-indices*)
    (gaussian-builder:set-up-posterior-funcs!))
  (gaussian-builder:construct-joint-sample!))

;; TODO register this function against type tag
(define (gaussian:marginal-likelihood rv)
  10) ;; TODO

;; internals

(define (gaussian-builder:construct-joint-sample!)
  (error "must be called after gaussian:construct-joint-sample!"))

(define ((gaussian-builder:get-conditioning-blocks) func)
  ((partition! (lambda (pair) (random-value:forced? (car pair)))
               (hash-table->alist *indices*))
   (lambda (sampled unsampled)
     ((partition! (lambda (pair) (memq (car pair) *sources-list*)) ;; TODO speed this up, hash set
                  unsampled)
      (lambda (unsampled-sources unsampled-derived)

        (define (get-dense-block indices)
          (let ((result (m:zeros (length indices) (length *sources-list*))))
            (let ((rows (vector-refs *cov-factor* indices))
                  (i 0))
              (vector-for-each (lambda (row)
                          (vector-map (lambda (colval)
                                        (matrix-set! result i (car colval) (cdr colval)))
                                      row)
                          (set! i (fix:+ i 1)))
                        rows))
            result))

        ;; actual computation!

        (set! *post-indices* (make-eq-hash-table))
        (let ((counter 0))
          (for-each (lambda (pair) (hash-table/put! *post-indices* (car pair) counter)
                      (set! counter (fix:+ counter 1)))
                    (append unsampled-sources sampled unsampled-derived)))

        (let ((sources-factor (get-dense-block (map cdr unsampled-sources)))
              (cond-factor (get-dense-block (map cdr sampled)))
              (derived-factor (get-dense-block (map cdr unsampled-derived)))

              (sources-mean (vector->col-matrix (vector-refs *means* (map cdr unsampled-sources))))
              (cond-mean (vector->col-matrix (vector-refs *means* (map cdr sampled))))
              (derived-mean (apply col-matrix (map cdr unsampled-derived)))

              (cond-obs (apply col-matrix (map random-value:force (map car sampled)))))
          (func sources-factor sources-mean cond-factor cond-mean cond-obs derived-factor derived-mean)))))))

;; TODO only do this calculation if it's not already done; otherwise it should
;; leave around enough to do the operations without re-calculation
(define (gaussian-builder:set-up-posterior-funcs!)
  ((gaussian-builder:get-conditioning-blocks)
     (lambda (sources-factor sources-mean cond-factor cond-mean cond-obs derived-factor derived-mean)
       (let* ((C cond-factor)
              (CT (m:transpose C))
              (CCT (matrix*matrix cond-factor CT))
              ;; TODO put computation in funcs, just set things to false here
              (post-innovations-mean (if (fix:> (m:num-rows cond-obs) 0)
                                       (matrix*matrix
                                         CT
                                         (solve-psd CCT
                                                    (matrix-matrix cond-obs cond-mean)))
                                       (m:zeros (m:num-cols C) 1)))
              (post-sources-mean (matrix+matrix (matrix*matrix sources-factor post-innovations-mean)
                                                sources-mean))
              (post-derived-mean (matrix+matrix (matrix*matrix derived-factor post-innovations-mean)
                                                derived-mean))
              (post-means (m:vstack post-sources-mean cond-obs post-derived-mean)))

         (set! gaussian:posterior-mean
           (lambda (rv)
             (hash-table/lookup *post-indices*
                                rv
                                (lambda (idx) (matrix-ref post-means idx 0))
                                (lambda () (error "invalid argument to gaussian:posterior-mean")))))

         (set! gaussian-builder:construct-joint-sample!
           (let ((saved-chol #f))
             (lambda ()
               (let* ((chol (if (or saved-chol (fix:= (m:num-rows cond-obs) 0))
                              saved-chol
                              (cholesky (matrix-matrix (m:eye (m:num-cols C))
                                                       (matrix*matrix CT (solve-psd CCT C))))))
                      (innovations-sample (let ((white (m:randn (m:num-cols C) 1)))
                                            (if chol (matrix*matrix chol white) white)))
                      (sources-sample (matrix+matrix (matrix*matrix sources-factor innovations-sample)
                                                     post-sources-mean))
                      (derived-sample (matrix+matrix (matrix*matrix derived-factor innovations-sample)
                                                     post-derived-mean))
                      (full-sample (m:vstack sources-sample cond-obs derived-sample)))
                 (set! saved-chol chol)
                 (hash-table/for-each *post-indices* (lambda (rv i)
                                                       (random-value:set-val! rv (matrix-ref full-sample i 0))
                                                       (random-value:set-handled! rv #t)))
                 ))))
         ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rollback interface for MH controller ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO



;; TESTING TODO REMOVE

(define x (gaussian 0 1))
(define y (gaussian 0 4))
;; (define e (gaussian 0 1))
(define z (gaussian:+ x y))
(random-value:force-set! z 3)

