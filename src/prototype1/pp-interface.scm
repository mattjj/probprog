(declare (usual-integrations))

;; this file contains the interface functions users can call

(load "randutil")

;;;;;;;;;;;;;;
;; SAMPLING ;;
;;;;;;;;;;;;;;

;; discrete

(define (discrete items #!optional weights proposer)
  (if (and (default-object? proposer)
           (not (default-object? weights))
           (procedure? weights))
    (let ((real-proposer weights))
      (set! weights proposer)
      (set! proposer real-proposer)))

  (sample
    'discrete
    discrete:rvs
    discrete:log-likelihood
    (discrete:make-params items weights)
    proposer))

;; continuous

(define (gaussian mean var #!optional proposer)
  (if (default-object? proposer)
    (set! proposer (proposals:additive-gaussian 0 (/ var 4))))

  (sample
    'gaussian
    gaussian:rvs
    gaussian:log-likelihood
    (gaussian:make-params mean var)
    proposer))

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

