;;;; Simple stack&queue Abstraction

(declare (usual-integrations))

(define-record-type <stack&queue>
    (%make-stack&queue front back)
    stack&queue?
  (front stack&queue-front set-stack&queue-front!)
  (back stack&queue-back set-stack&queue-back!))
  

(define (make-stack&queue)
  (%make-stack&queue '() '()))

(define (stack&queue-empty? stq)
  (not (pair? (stack&queue-front stq))))

(define (stack&queued? stq item)
  (memq item (stack&queue-front stq)))

(define (push! stq object)
  (if (pair? (stack&queue-front stq))
      (set-stack&queue-front! stq
        (cons object (stack&queue-front stq)))
      (begin
	(set-stack&queue-front! stq
	  (cons object (stack&queue-front stq)))
	(set-stack&queue-back! stq
	  (stack&queue-front stq))))
  unspecific)

(define (add-to-end! stq object)
  (let ((new (cons object '())))
    (if (pair? (stack&queue-back stq))
	(set-cdr! (stack&queue-back stq) new)
	(set-stack&queue-front! stq new))
    (set-stack&queue-back! stq new)
    unspecific))

(define (pop! stq)
  (let ((next (stack&queue-front stq)))
    (if (not (pair? next))
	(error "Empty stack&queue -- POP"))
    (if (pair? (cdr next))
	(set-stack&queue-front! stq (cdr next))
	(begin
	  (set-stack&queue-front! stq '())
	  (set-stack&queue-back! stq '())))
    (car next)))



