(declare (usual-integrations))

;;;;;;;;;;;;
;; ptrace ;;
;;;;;;;;;;;;

(define-record-type <ptrace>
                    (%ptrace:new choices likelihood-score emit-continuation doer-hook)
                    ptrace?
                    (choices ptrace:choices ptrace:set-choices!)
                    (likelihood-score ptrace:likelihood-score ptrace:set-likelihood-score!)
                    (emit-continuation ptrace:emit-continuation ptrace:set-emit-continuation!)
                    (doer-hook ptrace:doer-hook ptrace:set-doer-hook!))

(define (ptrace:set-all! pt1 pt2)
  (ptrace:set-choices! pt1 (ptrace:choices pt2))
  (ptrace:set-likelihood-score! pt1 (ptrace:likelihood-score pt2))
  (ptrace:set-emit-continuation! pt1 (ptrace:emit-continuation pt2))
  (ptrace:set-doer-hook! pt1 (ptrace:doer-hook pt2)))

(define (ptrace:new)
  (%ptrace:new '() #f #f #f))

(define (ptrace:length ptrace)
  (length (ptrace:choices ptrace)))

;; truncates up to and NOT including index
(define (ptrace:head! ptrace index)
  (let* ((oldlen (ptrace:length ptrace))
         (index (- oldlen index))
         (choices (list-tail (ptrace:choices ptrace) index)))
    (ptrace:set-choices! ptrace choices)
    (ptrace:set-likelihood-score! ptrace #f)
    (ptrace:set-emit-continuation! ptrace #f)
    ptrace))

(define (ptrace:add-choice! choice)
  (ptrace:set-choices! *current-ptrace* (cons choice (ptrace:choices *current-ptrace*))))

;;;;;;;;;;;;
;; choice ;;
;;;;;;;;;;;;

(define-record-type <choice>
                    (%choice:new name parameters log-likelihood proposer val
                                 forced continuation force-hook force-set-hook)
                    choice?
                    (name choice:name)
                    (parameters choice:parameters)
                    (log-likelihood choice:log-likelihood)
                    (proposer choice:proposer)
                    (val choice:val choice:set-val!)
                    (forced choice:forced? choice:set-forced!)
                    (continuation choice:continuation)
                    (force-hook choice:force-hook)
                    (force-set-hook choice:force-set-hook))

(define (choice:new name parameters log-likelihood proposer k #!optional force-hook force-set-hook)
  (if (default-object? force-hook) (set! force-hook (lambda () #!unspecific)))
  (if (default-object? force-set-hook) (set! force-set-hook (lambda () #!unspecific)))
  (%choice:new name parameters log-likelihood proposer 'unset #f k force-hook force-set-hook))

(define (choice:copy choice)
  (%choice:new (choice:name choice)
               (choice:parameters choice)
               (choice:log-likelihood choice)
               (choice:proposer choice)
               'unset
               (choice:forced? choice)
               (choice:continuation choice)
               (choice:force-hook choice)
               (choice:force-set-hook choice)))

(define (choice:force c)
  (if (not (choice? c))
    c
    (begin
      (choice:set-forced! c #t)
      ((choice:force-hook c))
      (choice:val c))))

(define (choice:force-set! c val)
  (let ((old-val (choice:val c)))
    (choice:set-forced! c #t)
    (choice:set-val! c val)
    ((choice:force-set-hook c))
    old-val))

