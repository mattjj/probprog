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

