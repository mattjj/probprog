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
  (let ((x (gaussian 0 1)))
    (emit x 1 (likelihood:additive-gaussian 0 0.1))
    x))

(define (dumb3)
  (let ((x (gaussian 0 1)))
    (emit x 1 (likelihood:additive-gaussian 0 0.1))
    (< x 0.5)))

;; run with (estimate-mean dumb4 100)
;; answer is x_prior_mean + x_var / (sum of all variances) * (obseved - emission_prior_mean)
;; with current settings, that's 2/3.5 which is about 0.5714
(define (dumb4)
  (let ((x (gaussian 0 1))
        (y (gaussian 0 2)))
    (emit (+ x y) 2 (likelihood:additive-gaussian 0 0.5))
    x))

(define (dumb5)
  (let ((label (pramb 0 1)))
    (emit (gaussian (* 2 label) 1) 2 (likelihood:additive-gaussian 0 0.2))
    label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for gathering statistics ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (estimate-indicator-probability pp-thunk nsamples)
  (let lp ((count 0)
           (iter 0))
    (if (< iter nsamples)
      (lp
        (+ count (if (pp-thunk) 1 0))
        (+ iter 1))
      (/ count nsamples))))

(define (estimate-mean pp-thunk nsamples)
  (let lp ((tot 0)
           (iter 0))
    (if (< iter nsamples)
      (lp
        (+ tot (pp-thunk))
        (+ iter 1))
      (/ tot nsamples))))

