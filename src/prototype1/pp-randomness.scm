(declare (usual-integrations))

;;;;;;;;;;;;;;
;; SAMPLING ;;
;;;;;;;;;;;;;;

;; discrete

(define (discrete weighted-list #!optional proposer)
  (sample
    'discrete
    discrete:rvs
    discrete:likelihood
    weighted-list
    proposer))

;; TODO make this a macro so that arguments are delayed
(define (pramb . args)
  (discrete (map (lambda (x) (cons 1 x)) args)))

;; continuous

(define (gaussian mean var #!optional proposer)
  (if (default-object? proposer)
    (set! proposer (proposals:additive-gaussian 0 (/ var 2))))

  (sample
    'gaussian
    gaussian:rvs
    gaussian:likelihood
    (cons mean var)
    proposer))

;;;;;;;;;;;;;;
;; EMITTING ;;
;;;;;;;;;;;;;;

(define ((likelihood:additive-gaussian mean var) x obs)
  (gaussian:likelihood obs (cons (+ mean x) var)))

(define (likelihood:exact x obs)
  (if (eq? x obs) 1 0))

;;;;;;;;;;;;;;;
;; PROPOSALS ;;
;;;;;;;;;;;;;;;

(define ((proposals:additive-gaussian mean var) choice)
  (let* ((params (cons mean var))
         (old-val (choice:val choice))
         (nudge (gaussian:rvs params))
         (new-val (+ old-val nudge))
         (proposal-score (gaussian:likelihood nudge params)))
    (set! *forward-score* proposal-score)
    (set! *backward-score* proposal-score)
    new-val))

