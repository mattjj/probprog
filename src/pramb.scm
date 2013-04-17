
;; Probabilistic amb extension for Scheme
;; Eyal Dechter, Matt Johnson, Zenna Tevares
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "utils")

(define-syntax pramb
  (sc-macro-transformer
   (lambda (form uenv)
     `(pramb-list
       (list ,@(map (lambda (arg)
		      `(lambda ()
			 ,(close-syntax arg uenv)))
		    (cdr form)))))))


;; choice points
;; choice-points is a list of choice points
;; a choice point is a list of thunks
(define *choice-points* (empty-choice-points))
(define (empty-choice-points) '())
(define (add-to-choice-points xs) (cons xs *choice-points*))


;; interface
;; functions: 
;;    sample <probabilistic function> <number of samples> --> <list of tuples (choices)> 

(define (pramb-list alternatives)
  (if (null? alternatives)
      (error "pramb-list: Why are there no alternatives?"))
  (call-with-current-continuation
   (lambda (k)
     (add-to-choice-points
      (map (lambda (alternative)
             (lambda ()
               (within-continuation k alternative)))
           alternatives))
     (yield))))

(define (with-uniform-mh-sampler thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let ((propose propose-uniform-mh)
                 (*choice-points* (empty-choice-points))
                 (*top-level* k))
       (thunk)))))




;;; Representation of the search schedule

(define *search-schedule*)

(define (empty-search-schedule)
  (make-stack&queue))

(define (yield)
  (if (-empty? *choice-points*)
      (*top-level* #f)
      ((uniform-choose (car *choice-points*))))

(define (force-next thunk)
  (push! *search-schedule* thunk))


;;; For incremental interactive experiments from REPL. 

(define (init-amb)
  (set! *search-schedule* (empty-search-schedule))
  (set! *number-of-calls-to-fail* 0)
  'done)

(define add-to-search-schedule ;; Default is depth 1st
  add-to-depth-first-search-schedule)

(define *top-level*
  (lambda (ignore)
    (display ";No more alternatives\n")
    (abort->top-level unspecific)))


(define (require p)
  (if (not p) (amb)))

