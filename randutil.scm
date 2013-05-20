(declare (usual-integrations))

;; NOTE: functions in this file use flo-typed math so they are a pain to call
;; directly

(load "constants")
(load "util")

;;;;;;;;;;;;;;
;; Gaussian ;;
;;;;;;;;;;;;;;

;; packaging parameters is convenient

(define (gaussian:make-params mean var)
  (flo:vector mean var))
(define (gaussian:mean params)
  (flo:vector-ref params 0))
(define (gaussian:var params)
  (flo:vector-ref params 1))

;; sampling

(define (gaussian:rvs params)
  (let ((mean (gaussian:mean params))
        (var (gaussian:var params)))
    (let ((u (random 1.0))
          (v (random 1.0))
          (std (flo:sqrt var)))
      (flo:+ mean
             (flo:* std
                    (flo:*
                      (flo:sqrt (flo:* -2. (flo:log u)))
                      (flo:cos (flo:* (flo:* 2. pi) v))))))))

;; log-likelihood

(define (gaussian:log-likelihood val params)
  (let* ((mean (flo:vector-ref params 0))
         (var (flo:vector-ref params 1))
         (centered (- val mean)))
    (flo:- (flo:/ (flo:* centered centered) (flo:* -2. var))
           (flo:/ (flo:log (flo:* (flo:* 2. pi) var)) 2.))))

;;;;;;;;;;;;;;
;; Discrete ;;
;;;;;;;;;;;;;;

;; packaging parameters is convenient

(define (discrete:make-params items #!optional weights)
  (let ((tot #f))
    (if (not (default-object? weights))
      (begin
        (set! weights (list->flo-vector weights))
        (set! tot (flo:vector-sum weights))))
    `#(,(list->vector items) ,weights ,tot)))

(define (discrete:items params)
  (vector-ref params 0))

(define (discrete:weights params)
  (vector-ref params 1))

(define (discrete:tot params)
  (vector-ref params 2))

;; sampling

;; if all weights are zero, always chooses first
(define (discrete:rvs params)
  (let ((items (discrete:items params))
        (weights (discrete:weights params))
        (tot (discrete:tot params)))
    (if (default-object? weights)
      (vector-ref items (random (vector-length items)))
      (let lp ((val (flo:* tot (random 1.0)))
               (idx 0))
        (let ((val (flo:- val (flo:vector-ref weights idx))))
          (if (not (flo:> val 0.))
            (vector-ref items idx)
            (lp val (+ idx 1))))))))

;;NOTE doesn't actually normalize
(define (discrete:log-likelihood val params)
  (let ((items (discrete:items params))
        (weights (discrete:weights params))
        (tot (discrete:tot params)))
    (if (default-object? weights)
      (flo:negate (log (vector-length items)))
      (let lp ((idx 0))
        (if (eq? val (vector-ref items idx))
          (flo:log (flo:/ (flo:vector-ref weights idx) tot))
          (lp (+ idx 1)))))))

