(declare (usual-integrations))

(load "pp")
(load "pp-interface")

;; try running with
;; (estimate-indicator-probability dumb2 100)
;; correct answer is 1/4. maybe decrease NUM-MH-STEPS.
(define (dumb2)
  (let ((x (discrete '(0 1)))
        (y (discrete '(0 1)))
        (z (discrete '(0 1))))
    (let ((sum (+ x y z)))
      (emit (or (= sum 0)
                (= sum 2))
            #t)
      (= sum 0))))

(define (dumb)
  (let ((x (gaussian 0 4)))
    (emit x 1 (likelihood:additive-gaussian 0 0.01))
    x))

(define (dumb3)
  (let ((x (gaussian 0 1)))
    (emit x 1 (likelihood:additive-gaussian 0 0.1))
    (< x 0.5)))

;; run with (estimate-mean dumb4 1000)
;; true posterior mean is 4*3/6 = 2
(define (dumb4)
  (let ((x (gaussian 0 1))
        (y (gaussian 0 4)))
    (emit (rv:+ x y) 3 (likelihood:additive-gaussian 0 1))
    y))

(define (dumb5)
  (let ((label (pramb 0 1)))
    (emit (gaussian (* 2 label) 1) 2 (likelihood:additive-gaussian 0 0.2))
    label))

