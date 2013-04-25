(declare (usual-integrations))

#| GAUSSIAN |#

(define pi (acos -1))

(define (gaussian:rvs params)
  (let ((mean (car params))
        (var (cadr params)))
    (let ((u (random 1.0))
          (v (random 1.0))
          (std (sqrt var)))
      (+ mean
        (* std
            (sqrt (- (* 2 (log u))))
            (cos (* 2 pi v)))))))

(define (gaussian:likelihood val params)
  (let ((mean (car params))
        (var (cadr params)))
    (let ((centered (- val mean)))
      (/ (exp (- (/ (* centered centered) var)))
         (sqrt (* 2 pi var))))))

#| DISCRETE/CATEGORICAL |#

;; if all weights are zero, always chooses first
;; weighted-list looks like '((3 hi) (1 bye) (7 foo))
(define (discrete:rvs weighted-list)
  (let ((tot (apply + (map car weighted-list))))
    (let lp ((val (* tot (random 1.0)))
             (lst weighted-list))
      (let* ((head (car lst))
             (val (- val (car head))))
        (if (<= val 0)
          (cadr head)
          (lp val (cdr lst)))))))

;;NOTE doesn't actually normalize
(define (discrete:likelihood val weighted-list)
  (car (find (lambda (elt) (eq? val (cadr elt)))
             weighted-list)))

