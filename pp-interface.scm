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

(define ((proposals:additive-gaussian mean var) rv)
  (let* ((params (gaussian:make-params mean var))
         (nudge (gaussian:rvs params))
         (proposal-score (gaussian:log-likelihood nudge params))
         (new-val (+ rv nudge)))
    (set! *forward-score* proposal-score)
    (set! *backward-score* proposal-score)
    new-val))

(define ((proposals:from-prior sampler likelihood) rv)
  (let ((new-val (sampler)))
    (let ((forward-score (likelihood new-val))
          (backward-score (likelihood rv)))
      (set! *forward-score* forward-score)
      (set! *backward-score* backward-score)
      new-val)))

