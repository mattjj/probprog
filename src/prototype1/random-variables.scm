(declare (usual-integrations))

#| GAUSSIAN |#

(define pi (acos -1)) ;; TODO is pi defined somewhere?

(define (gaussian:rvs params)
  (let ((mean (car params))
        (var (cdr params)))
    (let ((u (random 1.0))
          (v (random 1.0))
          (std (sqrt var)))
      (+ mean
         (* std
            (sqrt (- (* 2 (log u))))
            (cos (* 2 pi v)))))))

(define (gaussian:likelihood val params)
  (let ((mean (car params))
        (var (cdr params)))
    (let* ((std (sqrt var))
           (standardized (/ (- val mean) std)))
      (/ (exp (- (/ (* standardized standardized) 2)))
         (* std (sqrt (* 2 pi)))))))

#| DISCRETE/CATEGORICAL |#

;; if all weights are zero, always chooses first
;; weighted-list looks like '((3 . hi) (1 . bye) (7 . foo))
(define (discrete:rvs weighted-list)
  (let ((tot (apply + (map car weighted-list))))
    (let lp ((val (* tot (random 1.0)))
             (lst weighted-list))
      (let* ((head (car lst))
             (val (- val (car head))))
        (if (<= val 0)
          (cdr head)
          (lp val (cdr lst)))))))

;;NOTE doesn't actually normalize
(define (discrete:likelihood val weighted-list)
  (car (find (lambda (elt) (eq? val (cdr elt)))
             weighted-list)))

