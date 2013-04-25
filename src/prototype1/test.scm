(load "pp")

(define (dumb)
  (let ((x (gaussian 0 1)))
    (emit x 1 (likelihood:additive-gaussian 0 0.1))
    x))

;; try running with
;; (estimate-indicator-probability dumb2 100)
;; correct answer is 1/4. maybe decrease NUM-MH-STEPS.
(define (dumb2)
  (let ((x (pramb 0 1))
        (y (pramb 0 1))
        (z (pramb 0 1)))
    (let ((sum (+ x y z)))
      (emit (or (= sum 0) (= sum 2))
            #t
            likelihood:exact)
      (= sum 0))))

