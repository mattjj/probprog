(declare (usual-integrations))
(load "randutil")

(define (categorical items #!optional weights proposer)
  (if (and (default-object? proposer)
           (not (default-object? weights))
           (procedure? weights))
    (let ((real-proposer weights))
      (set! weights proposer)
      (set! proposer real-proposer)))

  (let ((params (categorical:make-params items weights)))
    (sample
      (lambda () (categorical:rvs params))
      (lambda (val) (categorical:log-likelihood val params))
      proposer)))

