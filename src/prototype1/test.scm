(load "pp")

;; try running with
;; (estimate-indicator-probability dumb2 100)
;; correct answer is 1/4. maybe decrease NUM-MH-STEPS.
(define (dumb2)
  (let ((x (pramb 0 1))
        (y (pramb 0 1))
        (z (pramb 0 1)))
    (let ((sum (+ x y z)))
      (emit (and (= x 1) (= y 1)) #t)
      (list x y z))))

(define (dumb)
  (let ((x (gaussian 0 1)))
    (emit x 1 (likelihood:additive-gaussian 0 0.1))
    x))

(define (dumb3)
  (let ((x (gaussian 0 1)))
    (emit x 1 (likelihood:additive-gaussian 0 0.1))
    (< x 0.5)))

;; run with (estimate-mean dumb4 100)
;; answer is 1.2
(define (dumb4)
  (let ((x (gaussian 0 1))
        (y (gaussian 0 2)))
    (emit (+ x y) 3 (likelihood:additive-gaussian 0 0.5))
    x))

(define (dumb5)
  (let ((label (pramb 0 1)))
    (emit (gaussian (* 2 label) 1) 2 (likelihood:additive-gaussian 0 0.2))
    label))

