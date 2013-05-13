(declare (usual-integrations))

;;;;;;;;;;;;
;; ptrace ;;
;;;;;;;;;;;;

(define-record-type <ptrace>
                    (%ptrace:new choices likelihood-score emit-continuation)
                    ptrace?
                    (choices ptrace:choices ptrace:set-choices!)
                    (likelihood-score ptrace:likelihood-score ptrace:set-likelihood-score!)
                    (emit-continuation ptrace:emit-continuation ptrace:set-emit-continuation!))

(define (ptrace:set-all! pt1 pt2)
  (ptrace:set-choices! pt1 (ptrace:choices pt2))
  (ptrace:set-likelihood-score! pt1 (ptrace:likelihood-score pt2))
  (ptrace:set-emit-continuation! pt1 (ptrace:emit-continuation pt2)))

(define (ptrace:new choices)
  (%ptrace:new choices #f #f))

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
                    (%choice:new proposer continuation random-val prior-score)
                    choice?
                    (proposer choice:proposer choice:set-proposer!)
                    (continuation choice:continuation choice:set-continuation!)
                    (random-val choice:random-val choice:set-random-val!)
                    (prior-score choice:prior-score choice:set-prior-score!))

(define (choice:new proposer continuation)
  (%choice:new proposer continuation 'unset 'unset))

(define (choice:copy choice)
  (choice:new (choice:proposer choice) (choice:continuation choice)))

;;;;;;;;;;;;;;;;;;
;; random value ;;
;;;;;;;;;;;;;;;;;;

(define-record-type <random-value>
                    (%random-value:new type val forced? handled?)
                    random-value?
                    (type random-value:type)
                    (val random-value:val random-value:set-val!)
                    (forced? random-value:forced? random-value:set-forced!)
                    (handled? random-value:handled? random-value:set-handled!))

(define (random-value:new type val)
  (%random-value:new type val #f #f))

(define (random-value:force rv)
  (if (random-value? rv)
    (begin
      (random-value:set-forced! rv #t)
      (random-value:val rv))
    rv))

(define (random-value:force-set! rv val)
  (let ((old-val (random-value:val rv)))
    (random-value:set-forced! rv #t)
    (random-value:set-val! rv val)
    old-val))

;; TODO epoch stamp on random values, maybe user can only call new so we can
;; hook timestamp in there
