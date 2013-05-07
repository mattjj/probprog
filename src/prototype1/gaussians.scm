(declare (usual-integrations))
(load "pp-interface")
(load "randutil")

;; TODO add to generic

(define (gaussian:+ . args)
  ((partition! gaussian-sample? args)
   (lambda (gaussians nongaussians)
     (gaussian-builder:new-sum gaussians (apply default:+ (map force-sample nongaussians))))))

(define (gaussian mean var #!optional proposer)
  (if (default-object? proposer)
    (set! proposer (proposals:additive-gaussian 0 (/ var 4))))

  (let ((params (gaussian:make-params mean var)))
    (sample
      gaussian:rvs
      params
      (lambda (k)
        (choice:new 'gaussian
                    params
                    gaussian:log-likelihood
                    proposer
                    k)))))

;; (ptrace:add-choice! (choice:new name parameters log-likelihood proposer k force-hook force-set-hook))
;; (define (sample name sampler parameters choice-constructor)
