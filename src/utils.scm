
(define (uniform-choose xs)
  (let ( (i (random (length xs))))
    (list-ref xs i)))

(define (flip p)
  (< (random 1.0) p))

