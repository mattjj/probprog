;;; FROM PSET 1
;;;; Traditional LISP property lists
;;; extended to work on any kind of eq? data structure.

(declare (usual-integrations))

;;; Property lists are a way of creating data that looks like a record
;;; structure without commiting to the fields that will be used until
;;; run time.  The use of such flexible structures is frowned upon by
;;; most computer scientists, because it is hard to statically
;;; determine the bounds of the behavior of a program written using
;;; this stuff.  But it makes it easy to write programs that confuse
;;; such computer scientists.  I personally find it difficult to write
;;; without such crutches.  -- GJS


(define eq-properties (make-eq-hash-table))

(define (eq-put! node property value)
  (let ((plist (hash-table/get eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (set-cdr! vcell value)
	  (hash-table/put! eq-properties node
			   (cons (cons property value)
				 plist)))))
  node)

(define (eq-get node property)
  (let ((plist (hash-table/get eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (cdr vcell)
	  #f))))

