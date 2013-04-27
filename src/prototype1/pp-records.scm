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

(define (ptrace:set-all! pt1 pt2)
  (ptrace:set-choices! pt1 (ptrace:choices pt2))
  (ptrace:set-prior-scores! pt1 (ptrace:prior-scores pt2))
  (ptrace:set-likelihood-score! pt1 (ptrace:likelihood-score pt2))
  (ptrace:set-emit-continuation! pt1 (ptrace:emit-continuation pt2)))

(define (ptrace:new choices scores)
  (%ptrace:new choices scores #f #f))

(define (ptrace:length ptrace)
  (length (ptrace:choices ptrace)))

;; truncates up to and NOT including index
(define (ptrace:head! ptrace index)
  (let* ((oldlen (ptrace:length ptrace))
         (index (- oldlen index))
         (choices (list-tail (ptrace:choices ptrace) index))
         (scores (list-tail (ptrace:prior-scores ptrace) index)))
    (ptrace:set-choices! ptrace choices)
    (ptrace:set-prior-scores! ptrace scores)
    ptrace))

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

