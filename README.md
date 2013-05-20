Below are two examples.


```scheme
(load "load")

;;;; set up a simple discrete test

(define (test1)
  (let ((x (discrete '(0 1)))
        (y (discrete '(0 1)))
        (z (discrete '(0 1))))
    (let ((sum (+ x y z)))
      (emit (or (= sum 0)
                (= sum 2))
            #t)
      (list x y z))))

;;;; inspect a few samples

(test1)

;Value 2: (1 1 0)

(test1)

;Value 3: (0 1 1)

(test1)

;Value 4: (0 0 0)

;;;; construct a stream of samples

(define ss (sample-stream test1 0 5))

(stream-head ss 10)

;Value: ((0 0 0) (0 0 0) (0 0 0) (1 1 0) (1 1 0) (1 1 0) (0 1 1) (0 1 1) (1 0 1) (1 1 0))

;;;; estimate the probability all are zero

(define (all-zero? s)
  (if (= (reduce + 0 s) 0)
    1
    0))

(estimate-mean (stream-map all-zero? ss) 2000)

;Value: 103/400

(estimate-mean (stream-map all-zero? ss) 4000)

;Value: 1/4

;;;; the true probability is 1/4
```

```scheme
(load "load")

;;;; set up a simple Gaussian test

(define (test3)
  (let ((x (gaussian 0 1))
        (y (gaussian 0 4)))
    (emit (+ x y) 3 (likelihood:additive-gaussian 0 1))
    y))

;;;; inspect a few samples

(test3)

;Value: 2.627738902766393

(test3)

;Value: 1.4713636283192144

(test3)

;Value: 3.650811672098934

(test3)

;Value: 2.508292233153293

;;;; construct a stream of samples from the Markov chain

(define ss (sample-stream test3 5000 25))

(stream-head ss 5)

;Value: (2.362360472725903 2.457989361328277 .9664606900582463 2.245335036134793 2.241072726252283)

;;;; estimate the mean

(estimate-mean ss 2000)

;Value: 2.0789872746395206

(estimate-mean ss 3000)

;Value: 2.0538287789004213

;;;; the true posterior mean is 2

```
