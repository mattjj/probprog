(declare (usual-integrations))

(load "pp-records.scm")
(load "random-variables.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals and initialization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BURNIN-MH-STEPS 200)
(define MH-STEPS 25)

(define *niter*)
(define *current-ptrace*)
(define *alternative-ptrace*)
(define *forward-score*)
(define *backward-score*)
(define *common-ptrace-prefix-length*)

(define (reset)
  (set! *niter* NUM-MH-STEPS)
  (set! *current-ptrace* (ptrace:new '() '()))
  (set! *alternative-ptrace* #f))
(reset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forward-sampling with bookkeeping ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sample name sampler scorer parameters proposer)
  (if (default-object? proposer)
    (set! proposer (prior-proposer sampler scorer parameters)))

  (let ((val (call-with-current-continuation
               (lambda (k)
                 (let ((val (sampler parameters)))
                   (ptrace:add-choice! (choice:new name parameters proposer val k))
                   val)))))
    (ptrace:add-prior-score! (scorer val parameters))
    val))

(define ((prior-proposer sampler scorer parameters) choice)
  (let ((new-val (sampler parameters))
        (old-val (choice:val choice)))
    (let ((forward-score (scorer new-val parameters))
          (backward-score (scorer old-val parameters)))
      (set! *forward-score* forward-score)   ;; forward means alternative -> current
      (set! *backward-score* backward-score) ;; backward means current -> alternative
      new-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
;; Emit and MH over traces ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;:

(define (emit var observed-value #!optional likelihood-function)
  (if (default-object? likelihood-function)
    (set! likelihood-function likelihood:exact))

  (ptrace:set-likelihood-score! *current-ptrace* (likelihood-function var observed-value))
  (call-with-current-continuation
    (lambda (k)
      (ptrace:set-emit-continuation! *current-ptrace* k)
      (choose-ptrace (MH-sample-ptrace))))
  (if (> *niter* 0)
    (try-another)
    (reset)))

(define (MH-sample-ptrace)
  (if (not *alternative-ptrace*)
    *current-ptrace*
    (let ((forward-choice-prob (/ 1 (ptrace:length *alternative-ptrace*)))
          (backward-choice-prob (/ 1 (ptrace:length *current-ptrace*)))
          (current-prior (prior-score *current-ptrace*))
          (alternative-prior (prior-score *alternative-ptrace*))
          (current-likelihood (ptrace:likelihood-score *current-ptrace*))
          (alternative-likelihood (ptrace:likelihood-score *alternative-ptrace*)))
      (cond ((= alternative-likelihood 0) *current-ptrace*)
            ((= current-likelihood 0) *alternative-ptrace*)
            (else (let ((accept-current-probability
                          (* (/ current-prior alternative-prior)
                             (/ current-likelihood alternative-likelihood)
                             (/ backward-choice-prob forward-choice-prob)
                             (/ *backward-score* *forward-score*))))
                    (if (< (random 1.0) accept-current-probability)
                      *current-ptrace*
                      *alternative-ptrace*)))))))

(define (prior-score ptrace)
  (list-ref (reverse (ptrace:prior-scores ptrace)) *common-ptrace-prefix-length*))

(define (choose-ptrace ptrace)
  (set! *current-ptrace* ptrace)
  ((ptrace:emit-continuation ptrace) #!unspecific))

(define (try-another)
  (set! *niter* (- *niter* 1))
  (set! *alternative-ptrace* *current-ptrace*)
  (let* ((proposal-index (random (ptrace:length *current-ptrace*)))
         (new-ptrace (ptrace:head *current-ptrace* proposal-index))
         (choice (list-ref (reverse (ptrace:choices *current-ptrace*)) proposal-index))
         (k (choice:continuation choice)))
    (set! *common-ptrace-prefix-length* (ptrace:length new-ptrace))
    (set! *current-ptrace* new-ptrace)
    (within-continuation k (lambda () (propose choice)))))

(define (propose choice)
  (let ((new-val ((choice:proposer choice) choice))
        (new-choice (choice:copy choice)))
    (choice:set-val! new-choice new-val)
    (ptrace:add-choice! new-choice)
    new-val))

