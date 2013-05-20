(declare (usual-integrations))
(load "randutil")

(define (gaussian mean var #!optional proposer)
  (if (default-object? proposer)
    (set! proposer (proposals:additive-gaussian 0 (/ var 4))))

  (let ((params (gaussian:make-params mean var)))
    (sample
      (lambda () (gaussian:rvs params))
      (lambda (rv) (gaussian:log-likelihood rv params))
      proposer)))


(define (discrete items #!optional weights proposer)
  (if (and (default-object? proposer)
           (not (default-object? weights))
           (procedure? weights))
    (let ((real-proposer weights))
      (set! weights proposer)
      (set! proposer real-proposer)))

  (let ((params (discrete:make-params items weights)))
    (let ((sampler (lambda () (discrete:rvs params)))
          (likelihood (lambda (val) (discrete:log-likelihood val params))))
      (if (default-object? proposer)
        (set! proposer (proposals:from-prior sampler likelihood)))

    (sample sampler likelihood proposer))))

