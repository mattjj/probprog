(declare (usual-integrations))

;; converts into a flo-vector from a pair, list, or vector
(define (make-flo-vector stuff)
  (cond ((vector? stuff)
         (let* ((len (vector-length stuff))
                (v (flo:vector-cons len)))
           (let lp ((idx 0))
             (if (< idx len)
               (begin
                 (flo:vector-set! v idx (exact->inexact (vector-ref stuff idx)))
                 (lp (+ idx 1)))
               v))))
        ((list? (cdr stuff))
         (let* ((len (length stuff))
                (v (flo:vector-cons len)))
           (let lp ((lst stuff)
                    (idx 0))
             (if (< idx len)
               (begin
                 (flo:vector-set! v idx (exact->inexact (car lst)))
                 (lp (cdr lst) (+ idx 1)))
               v))))
        (else
          (let ((v (flo:vector-cons 2)))
            (flo:vector-set! v 0 (exact->inexact (car stuff)))
            (flo:vector-set! v 1 (exact->inexact (cdr stuff)))
            v))))

(define (flo:vector-sum v)
  (let ((len (flo:vector-length v)))
    (let lp ((idx 0)
             (tot 0.))
      (if (< idx len)
        (lp (+ idx 1) (flo:+ tot (flo:vector-ref v idx)))
        tot))))

(define (flo:sum . args)
  (let lp ((tot 0.)
           (lst args))
    (if (null? lst)
      tot
      (lp (flo:+ tot (car lst)) (cdr lst)))))
