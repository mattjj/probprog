(declare (usual-integrations))

;;;;;;;;;;;;
;; ptrace ;;
;;;;;;;;;;;;

(define-record-type <ptrace>
                    (%ptrace:new choices prior-scores likelihood-score emit-continuation)
                    ptrace?
                    (choices ptrace:choices ptrace:set-choices!)
                    (prior-scores ptrace:prior-scores ptrace:set-prior-scores!)
                    (likelihood-score ptrace:likelihood-score ptrace:set-likelihood-score!)
                    (emit-continuation ptrace:emit-continuation ptrace:set-emit-continuation!))

(define (ptrace:new choices scores)
  (%ptrace:new choices scores #f #f))

(define (ptrace:length ptrace)
  (length (ptrace:choices ptrace)))

;; up to and NOT including index
(define (ptrace:head ptrace index)
  (let ((oldlen (ptrace:length ptrace)))
    (ptrace:new
        (list-tail (ptrace:choices ptrace) (- oldlen index))
        (list-tail (ptrace:prior-scores ptrace) (- oldlen index)))))

(define (ptrace:add-choice! choice)
  (ptrace:set-choices! *current-ptrace* (cons choice (ptrace:choices *current-ptrace*))))

(define (ptrace:add-prior-score! score)
  (ptrace:set-prior-scores! *current-ptrace* (cons score (ptrace:prior-scores *current-ptrace*))))

;;;;;;;;;;;;
;; choice ;;
;;;;;;;;;;;;

(define-record-type <choice>
                    (choice:new name parameters proposer val continuation)
                    choice?
                    (name choice:name choice:set-name!)
                    (parameters choice:parameters choice:set-parameters!)
                    (proposer choice:proposer choice:set-proposer!)
                    (val choice:val choice:set-val!)
                    (continuation choice:continuation choice:set-continuation!))

(define (choice:copy choice)
  (choice:new
    (choice:name choice)
    (choice:parameters choice)
    (choice:proposer choice)
    (choice:val choice)
    (choice:continuation choice)))

