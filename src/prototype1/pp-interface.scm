(declare (usual-integrations))
(load "randutil")

;;;;;;;;;;;;;;
;; EMITTING ;;
;;;;;;;;;;;;;;

(define ((likelihood:additive-gaussian mean var) x obs)
  (gaussian:log-likelihood (exact->inexact obs) (gaussian:make-params (+ mean x) var)))

(define (likelihood:exact x obs)
  (if (eq? x obs) 0. neginf))

;;;;;;;;;;;;;;;
;; PROPOSALS ;;
;;;;;;;;;;;;;;;

(define ((proposals:additive-gaussian mean var) choice)
  (let* ((params (gaussian:make-params mean var))
         (old-val (choice:val choice))
         (nudge (gaussian:rvs params))
         (new-val (flo:+ old-val nudge))
         (proposal-score (gaussian:log-likelihood nudge params)))
    (set! *forward-score* proposal-score)
    (set! *backward-score* proposal-score)
    new-val))

(define ((proposals:prior-proposer sampler log-likelihood parameters) choice)
  (let ((new-val (sampler parameters))
        (old-val (choice:val choice)))
    (let ((forward-score (log-likelihood new-val parameters))
          (backward-score (log-likelihood old-val parameters)))
      (set! *forward-score* forward-score)   ;; forward means alternative -> current
      (set! *backward-score* backward-score) ;; backward means current -> alternative
      new-val)))
