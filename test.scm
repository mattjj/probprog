(declare (usual-integrations))

(load "pp")
(load "pp-interface")
(load "rvs")

(define (test1)
  (let ((x (discrete '(0 1)))
        (y (discrete '(0 1)))
        (z (discrete '(0 1))))
    (let ((sum (+ x y z)))
      (emit (or (= sum 0)
                (= sum 2))
            #t)
      (= sum 0))))

(define (test2)
  (let ((x (gaussian 0 1)))
    (emit x 1 (likelihood:additive-gaussian 0 0.1))
    (< x 0.5)))

;; true posterior mean is 4*3/6 = 2
(define (test3)
  (let ((x (gaussian 0 1))
        (y (gaussian 0 4)))
    (emit (+ x y) 3 (likelihood:additive-gaussian 0 1))
    y))

