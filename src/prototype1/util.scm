(declare (usual-integrations))

;;;;;;;;;;;;;
;; general ;;
;;;;;;;;;;;;;

(define ((g:sigma op id) f low high)
  (if (fix:> low high)
    id
    (let lp ((i (fix:+ low 1)) (sum (f low)))
      (if (fix:> i high)
        sum
        (lp (fix:+ i 1) (op sum (f i)))))))

(define sigma (g:sigma + 0))

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

