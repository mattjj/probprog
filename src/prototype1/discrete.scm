(declare (usual-integrations))
(load "pp-interface")
(load "randutil")

;; discrete

(define (discrete items #!optional weights proposer)
  (if (and (default-object? proposer)
           (not (default-object? weights))
           (procedure? weights))
    (let ((real-proposer weights))
      (set! weights proposer)
      (set! proposer real-proposer)))

  (if (default-object? proposer)
    (set! proposer (proposals:prior-proposer discrete:rvs discrete:log-likelihood
                                             (discrete:make-params item weights))))

  (sample
    discrete:rvs
    (discrete:make-params items weights)
    (lambda (k)
      (choice:new 'gaussian
                  params
                  gaussian:log-likelihood
                  proposer
                  k))))

