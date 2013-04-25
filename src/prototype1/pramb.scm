#| UTIL |#

(define (random:choice lst)
  (list-ref lst (random (length lst))))

;; NOTE: if all weights are zero, always returns the first one
(define (random:weighted-choice weighted-list)
  (let ((tot (apply + (map car weighted-list))))
    (if (= tot 0)
      (car weighted-list)
      (let lp ((val (* tot (random 1.0)))
               (lst weighted-list))
        (let ((val (- val (caar lst))))
          (if (< val 0)
            (car lst)
            (lp val (cdr lst))))))))

#| MAIN MACHINERY |#

(define-syntax pramb
  (sc-macro-transformer
    (lambda (form uenv)
      `(pramb-list
         (list ,@(map (lambda (arg)
                        `(lambda ()
                           ,(close-syntax arg uenv)))
                      (cdr form)))))))

(define (pramb-list alternatives)
  (call-with-current-continuation
    (lambda (k)
      (add-to-choices
        (map (lambda (alternative)
               (lambda ()
                 (within-continuation k alternative)))
             alternatives))
      ((random:choice alternatives))))) ;; TODO sample and create dynamic context score

(define (add-to-choices stuff)
  (set! *all-choices* (cons stuff *all-choices*)))

(define (set-primary! k score)
  (set! *primary-continuation* k)
  (set! *primary-score* score))

(define (emit var observed-value)
  (let ((my-score (if (eq? var observed-value) 1 0)))
    (call-with-current-continuation
      (lambda (k)
        (apply (lambda (score future)
                 (set-primary! future score)
                 (future #f))
               (random:weighted-choice `((,my-score ,k)
                                         (,*primary-score* ,*primary-continuation*)))))))
  (if (> *niter* 0)
    (begin
      (set! *niter* (- *niter* 1))
      ((random:choice (random:choice *all-choices*))))
    (begin
      (pp (length *all-choices*))
      (init))))
    ;;(init))) ;; NOTE: calling init here means no more emits can be encountered w/o an infinite loop

(define *niter*)
(define *all-choices*)
(define *primary-continuation*)
(define *primary-score*)

(define (init)
  (set! *niter* 100)
  (set! *primary-continuation* #f)
  (set! *primary-score* 0)
  (set! *all-choices* '())
  'ok)
(init)

#| DUMB PROGRAM |#

(define (dumb)
  (let ((x (pramb 0 1)))
    (let ((y (pramb 0 1)))
      (emit x 1) ;; probably revise this syntax, it's like require though, neato!
      (+ y x)))) ;; this statement is evalutated with a sample from the posterior!

(define (dumb2)
  (let ((x (pramb 0 1))
        (y (pramb 0 1))
        (z (pramb 0 1)))
    (emit (or (= 0 (+ x y z))
              (= 2 (+ x y z)))
          #t)
    (= 0 (+ x y z))))

(define (indicator-probability thunk nsamples)
  (let lp ((count 0)
           (iter 0))
    (if (< iter nsamples)
      (lp
        (+ count (if (thunk) 1 0))
        (+ iter 1))
      (/ count nsamples))))

#|
 | TODO
 | * DUMB im building up HUGE lists of all possibilities! dumb!
 |
 | * maybe i can pass the niter as the output to the call/cc in emit
 | * dynamic context
 | * non-uniform priors
 | * non-uniform proposals/scores. they should look at some kind of RECORD that
 |   includes the vals in discrete stuff. always collect prior score!
 |#
