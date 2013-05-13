(declare (usual-integrations))
(load "util")

(define matrix-type-tag '*matrix*)

(define (tag-matrix nrows ncols flovec)
  (list matrix-type-tag (cons nrows ncols) flovec))

(define (m:num-rows matrix)
  (caadr matrix))

(define (m:num-cols matrix)
  (cdadr matrix))

(define (matrix->flovec matrix)
  (caddr matrix))

; the next three functions set row-major order

(define (flatidx->rowidx flatidx nrows ncols)
  (fix:quotient flatidx ncols))

(define (flatidx->colidx flatidx nrows ncols)
  (fix:remainder flatidx ncols))

(define (rowcolidx->flatidx i j m)
  (fix:+ (fix:* i (m:num-cols m)) j))


(define (matrix-ref m i j)
  (flo:vector-ref (matrix->flovec m) (rowcolidx->flatidx i j m)))

(define (matrix-set! m i j v)
  (flo:vector-set! (matrix->flovec m) (rowcolidx->flatidx i j m) v))

(define (m:generate nrows ncols proc)
  (tag-matrix nrows ncols (flo:make-initialized-vector
                            (fix:* nrows ncols)
                            (lambda (flatidx)
                              (proc (flatidx->rowidx flatidx nrows ncols)
                                    (flatidx->colidx flatidx nrows ncols))))))

(define (m:zeros nrows ncols)
  (m:generate nrows ncols (lambda (i j) 0.)))

(define (m:ones nrows ncols)
  (m:generate nrows ncols (lambda (i j) 1.)))

(define (m:eye n)
  (m:generate n n (lambda (i j)
                    (if (fix:= i j)
                      1.
                      0.))))

(define (m:empty nrows ncols)
  (tag-matrix nrows ncols (flo:vector-cons (fix:* nrows ncols))))

(define (matrix-by-rows . rows)
  (matrix-by-row-list rows))

(define (matrix-by-row-list rows)
  (let ((nrows (length rows)))
    (let ((ncols (length (car rows))))
      (m:generate nrows ncols
                  (lambda (i j)
                    (exact->inexact (list-ref (list-ref rows i) j)))))))

(define (matrix-by-cols . cols)
  (matrix-by-col-list cols))

(define (matrix-by-col-list cols)
  (let ((ncols (length cols)))
    (let ((nrows (length (car cols))))
      (m:generate nrows ncols
                  (lambda (i j)
                    (exact->inexact (list-ref (list-ref cols j) i)))))))

(define (col-matrix . args)
  (m:generate (length args) 1 (lambda (i j)
                                (exact->inexact (list-ref args i)))))

(define (row-matrix . args)
  (m:generate 1 (length args) (lambda (i j)
                                (exact->inexact (list-ref args j)))))

(define (diag . args)
  (m:generate (length args) (length args)
              (lambda (i j)
                (if (fix:= i j)
                  (exact->inexact (list-ref args i))
                  0.))))

(define (m:transpose m)
  (m:generate (m:num-cols m) (m:num-rows m)
              (lambda (i j) (matrix-ref m j i))))

(define (matrix*matrix matrix1 matrix2)
  (let ((m1r (m:num-rows matrix1))
        (m1c (m:num-cols matrix1))
        (m2c (m:num-cols matrix2)))
    (let ((m1cm1 (fix:- m1c 1)))
      (m:generate m1r m2c
                  (lambda (i j)
                    (flo:sigma
                      (lambda (k)
                        (flo:* (matrix-ref matrix1 i k)
                               (matrix-ref matrix2 k j)))
                      0
                      m1cm1))))))

(define (matrix-binary-componentwise binop matrix1 matrix2)
  (let ((nrows (m:num-rows matrix1))
        (ncols (m:num-cols matrix1)))
    (m:generate nrows ncols
                (lambda (i j)
                  (binop (matrix-ref matrix1 i j)
                         (matrix-ref matrix2 i j))))))

(define (matrix+matrix matrix1 matrix2)
  (matrix-binary-componentwise flo:+ matrix1 matrix2))

(define (matrix-matrix matrix1 matrix2)
  (matrix-binary-componentwise flo:- matrix1 matrix2))


(define (solve-psd m b)
  (let ((chol (cholesky m)))
    (solve-upper-trinagular (m:transpose chol)
                            (solve-lower-triangular chol b))))

(define (solve-lower-triangular L b)
  (let ((nrows (m:num-rows b))
        (ncols (m:num-cols b)))
    (let ((result (m:empty nrows ncols)))
      (let outerloop ((k 0)) ;; across columns of b
        (if (fix:< k ncols)
          (let lp1 ((i 0))
            (if (fix:< i nrows)
              (let ((thesum (flo:sigma (lambda (j) (flo:* (matrix-ref L i j)
                                                          (matrix-ref result j k)))
                                       0 (fix:- i 1)))
                    (bi (matrix-ref b i k))
                    (lii (matrix-ref L i i)))
                (matrix-set! result i k (flo:/ (flo:- bi thesum) lii))
                (lp1 (fix:+ i 1))))
            (outerloop (fix:+ k 1)))))
      result)))

(define (solve-upper-triangular LT b)
  (let ((nrows (m:num-rows b))
        (ncols (m:num-cols b)))
    (let ((result (m:empty nrows ncols)))
      (let outerloop ((k 0)) ;; across columns of b
        (if (fix:< k ncols)
          (let lp1 ((i (fix:- nrows 1)))
            (let ((thesum (flo:sigma (lambda (j) (flo:* (matrix-ref LT i j)
                                                        (matrix-ref result j k)))
                                     (fix:+ i 1) (fix:- nrows 1)))
                  (bi (matrix-ref b i k))
                  (lii (matrix-ref LT i i)))
              (matrix-set! result i k (flo:/ (flo:- bi thesum) lii))
              (if (fix:> i 0) (lp1 (fix:- i 1)))))
          (outerloop (fix:+ k 1))))
      result)))

(define (cholesky A)
  (let* ((n (m:num-rows A))
         (L (m:empty n n)))
    (let ilp ((i 0))
      (if (fix:< i n)
        (begin
          (let ((thesum (flo:sigma (lambda (k) (flo:* (matrix-ref L i k)
                                                      (matrix-ref L i k)))
                                   0 (fix:- i 1)))
                (Aii (matrix-ref A i i)))
            (matrix-set! L i i (flo:sqrt (flo:- Aii thesum))))

          (let jlp ((j (fix:+ i 1)))
            (if (fix:< j n)
              (let ((thesum (flo:sigma (lambda (k) (flo:* (matrix-ref L i k)
                                                          (matrix-ref L j k)))
                                       0 (fix:- i 1)))
                    (Aij (matrix-ref A i j))
                    (Lii (matrix-ref L i i)))
                (matrix-set! L j i (flo:/ (flo:- Aij thesum) Lii))
                (jlp (fix:+ j 1)))))

            (ilp (fix:+ i 1)))))
    L))

