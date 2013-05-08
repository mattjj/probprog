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

  (let ((params (discrete:make-params items weights)))
    (sample
      (lambda () (discrete:rvs params))
      (lambda (val) (discrete:log-likelihood val params))
      proposer)))

;; continuous

(define (gaussian mean var #!optional proposer)
  (if (default-object? proposer)
    (set! proposer (proposals:additive-gaussian 0 (/ var 4))))

  (let ((params (gaussian:make-params mean var)))
    (sample
      (lambda () (gaussian:rvs params))
      (lambda (val) (gaussian:log-likelihood val params))
      proposer)))

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

(define ((proposals:additive-gaussian mean var) val)
  (let* ((params (gaussian:make-params mean var))
         (nudge (gaussian:rvs params))
         (new-val (+ val nudge))
         (proposal-score (gaussian:log-likelihood nudge params)))
    (set! *forward-score* proposal-score)
    (set! *backward-score* proposal-score)
    new-val))

(define ((proposals:from-prior sampler log-likelihood parameters) val)
  (let ((new-val (sampler parameters)))
    (let ((forward-score (log-likelihood new-val parameters))
          (backward-score (log-likelihood val parameters)))
      (set! *forward-score* forward-score)   ;; forward means alternative -> current
      (set! *backward-score* backward-score) ;; backward means current -> alternative
      new-val)))

