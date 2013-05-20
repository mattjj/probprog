(declare (usual-integrations))

;;;;;;;;;;;;;
;; general ;;
;;;;;;;;;;;;;

(define (vector-for-each proc vec)
  (let ((len (vector-length vec)))
    (let lp ((i 0))
      (if (fix:< i len)
        (begin
          (proc (vector-ref vec i))
          (lp (fix:+ i 1)))))))

(define (vector-refs vec ks)
  (vector-map (lambda (k) (vector-ref vec k)) (list->vector ks)))

(define (list-refs lst ks)
  ;; this procedure is complicated because we only want to traverse lst once
  (let* ((num (length ks))
         (kvec (list->vector ks))
         (kindices (sort! (make-initialized-vector num (lambda (i) i))
                          (lambda (i j) (fix:< (vector-ref kvec i) (vector-ref kvec j)))))
         (kvec (sort! kvec fix:<))
         (result (make-vector num)))
    (let lp ((list-index 0)
             (result-index 0)
             (lst lst))
      (if (fix:= result-index num)
        result
        (if (fix:= list-index (vector-ref kvec result-index))
          (begin (vector-set! result (vector-ref kindices result-index) (car lst))
                 (lp (fix:+ list-index 1) (fix:+ result-index 1) (cdr lst)))
          (lp (fix:+ list-index 1) result-index (cdr lst)))))))

(define ((g:sigma op id) f low high)
  (if (fix:> low high)
    id
    (let lp ((i (fix:+ low 1)) (sum (f low)))
      (if (fix:> i high)
        sum
        (lp (fix:+ i 1) (op sum (f i)))))))

(define sigma (g:sigma + 0))

(define (vector-sum v)
  (sigma (lambda (idx) (vector-ref v idx)) 0 (fix:- (vector-length v) 1)))

;;;;;;;;;;;;;;;;;
;; float stuff ;;
;;;;;;;;;;;;;;;;;

(define flo:sigma (g:sigma flo:+ 0.))

(define (flo:vector . args)
  (list->flo-vector args))

(define (flo:make-initialized-vector n proc)
  (let ((result (flo:vector-cons n)))
    (let lp ((i 0))
      (if (fix:< i n)
        (begin
          (flo:vector-set! result i (proc i))
          (lp (fix:+ i 1)))
        result))))

(define (flo:vector-sum v)
  (let ((len (flo:vector-length v)))
    (let lp ((idx 0)
             (tot 0.))
      (if (< idx len)
        (lp (+ idx 1) (flo:+ tot (flo:vector-ref v idx)))
        tot))))

(define (flo:sum . args)
  (let lp ((tot 0.)
           (args args))
    (if (null? args)
      tot
      (lp (flo:+ tot (car args)) (cdr args)))))

(define (list->flo-vector lst)
  (let* ((len (length lst))
         (v (flo:vector-cons len)))
    (let lp ((lst lst)
             (idx 0))
      (if (< idx len)
        (begin
          (flo:vector-set! v idx (exact->inexact (car lst)))
          (lp (cdr lst) (+ idx 1)))
        v))))

(define (vector->flo-vector vec)
  (let* ((len (vector-length vec))
         (v (flo:vector-cons len)))
    (let lp ((idx 0))
      (if (< idx len)
        (begin
          (flo:vector-set! v idx (exact->inexact (vector-ref vec idx)))
          (lp (+ idx 1)))
        v))))

(define (pair->flo-vector pair)
  (let ((v (flo:vector-cons 2)))
    (flo:vector-set! v 0 (exact->inexact (car pair)))
    (flo:vector-set! v 1 (exact->inexact (cdr pair)))
    v))

;;;;;;;;;;;;;
;; streams ;;
;;;;;;;;;;;;;

(define (stream-head-fold-left proc initial stream k)
  (if (= k 0)
    initial
    (stream-head-fold-left proc (proc (stream-car stream) initial)
                            (stream-cdr stream) (- k 1))))

