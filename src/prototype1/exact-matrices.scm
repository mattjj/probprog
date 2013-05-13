(declare (usual-integrations))
(load "util")
(load "randutil")

(define matrix-type-tag '*matrix*)

(define (tag-matrix nrows ncols vec)
  (list matrix-type-tag (cons nrows ncols) vec))

(define (m:num-rows matrix)
  (caadr matrix))

(define (m:num-cols matrix)
  (cdadr matrix))

(define (matrix->vec matrix)
  (caddr matrix))

; the next three functions set row-major order

(define (flatidx->rowidx flatidx nrows ncols)
  (fix:quotient flatidx ncols))

(define (flatidx->colidx flatidx nrows ncols)
  (fix:remainder flatidx ncols))

(define (rowcolidx->flatidx i j m)
  (fix:+ (fix:* i (m:num-cols m)) j))


(define (matrix-ref m i j)
  (vector-ref (matrix->vec m) (rowcolidx->flatidx i j m)))

(define (matrix-set! m i j v)
  (vector-set! (matrix->vec m) (rowcolidx->flatidx i j m) v))

(define (m:generate nrows ncols proc)
  (tag-matrix nrows ncols (make-initialized-vector
                            (fix:* nrows ncols)
                            (lambda (flatidx)
                              (proc (flatidx->rowidx flatidx nrows ncols)
                                    (flatidx->colidx flatidx nrows ncols))))))

(define (m:for-each proc m)
  (let ((nrows (m:num-rows m))
        (ncols (m:num-cols m)))
    (let ilp ((i 0))
      (if (fix:< i nrows)
        (begin
          (let jlp ((j 0))
            (if (fix:< j ncols)
              (begin (proc i j (matrix-ref m i j)) (jlp (fix:+ j 1)))))
          (ilp (fix:+ i 1)))))))

(define (m:zeros nrows ncols)
  (m:generate nrows ncols (lambda (i j) 0)))

(define (m:ones nrows ncols)
  (m:generate nrows ncols (lambda (i j) 1)))

(define (m:eye n)
  (m:generate n n (lambda (i j)
                    (if (fix:= i j)
                      1
                      0))))

(define (m:randn nrows ncols)
  (m:generate nrows ncols (lambda (i j) (gaussian:rvs (gaussian:make-params 0 1)))))

(define (m:empty nrows ncols)
  (tag-matrix nrows ncols (make-vector (fix:* nrows ncols))))

(define (m:diag . args)
  (m:generate (length args) (length args)
              (lambda (i j)
                (if (fix:= i j)
                  (list-ref args i)
                  0))))

(define (m:transpose m)
  (m:generate (m:num-cols m) (m:num-rows m)
              (lambda (i j) (matrix-ref m j i))))

(define (m:vstack . args)
  (if (fix:= (length args) 0)
    (error "vstack needs arguments"))
  (let* ((ncols (m:num-cols (car args)))
         (nrowss (map m:num-rows args))
         (nrows (reduce fix:+ 0 nrowss))
         (result (m:empty nrows ncols))
         (offset 0))
    (for-each (lambda (m)
                (m:for-each
                  (lambda (i j v) (matrix-set! result (fix:+ offset i) j v))
                  m)
                (set! offset (fix:+ offset (m:num-rows m))))
              args)
    result))

(define (matrix-by-rows . rows)
  (matrix-by-row-list rows))

(define (matrix-by-row-list rows)
  (let ((nrows (length rows)))
    (let ((ncols (length (car rows))))
      (m:generate nrows ncols
                  (lambda (i j)
                    (list-ref (list-ref rows i) j))))))

(define (matrix-by-cols . cols)
  (matrix-by-col-list cols))

(define (matrix-by-col-list cols)
  (let ((ncols (length cols)))
    (let ((nrows (length (car cols))))
      (m:generate nrows ncols
                  (lambda (i j)
                    (list-ref (list-ref cols j) i))))))

(define (col-matrix . args)
  (m:generate (length args) 1 (lambda (i j)
                                (list-ref args i))))

(define (vector->col-matrix v)
  (tag-matrix (vector-length v) 1 v))

(define (row-matrix . args)
  (m:generate 1 (length args) (lambda (i j)
                                (list-ref args j))))

(define (matrix*matrix matrix1 matrix2)
  (let ((m1r (m:num-rows matrix1))
        (m1c (m:num-cols matrix1))
        (m2c (m:num-cols matrix2)))
    (let ((m1cm1 (fix:- m1c 1)))
      (m:generate m1r m2c
                  (lambda (i j)
                    (sigma
                      (lambda (k)
                        (* (matrix-ref matrix1 i k)
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
  (matrix-binary-componentwise + matrix1 matrix2))

(define (matrix-matrix matrix1 matrix2)
  (matrix-binary-componentwise - matrix1 matrix2))


(define (solve-psd m b)
  (let ((pair (LDLT m)))
    (let ((L (car pair))
          (d (cdr pair)))
      (solve-upper-triangular
        (m:transpose L)
        (solve-diagonal
          d
          (solve-lower-triangular L b))))))

(define (solve-diagonal d b)
  (let ((nrows (m:num-rows b))
        (ncols (m:num-cols b)))
    (let ((result (m:empty nrows ncols)))
      (let outerloop ((k 0))
        (if (fix:< k ncols)
          (let lp1 ((i 0))
            (if (fix:< i nrows)
              (let ((di (matrix-ref d i 0))
                    (bik (matrix-ref b i k)))
                (matrix-set! result i k (/ bik di))
                (lp1 (fix:+ i 1))))
            (outerloop (fix:+ k 1)))))
      result)))

(define (solve-lower-triangular L b)
  (let ((nrows (m:num-rows b))
        (ncols (m:num-cols b)))
    (let ((result (m:empty nrows ncols)))
      (let outerloop ((k 0)) ;; across columns of b
        (if (fix:< k ncols)
          (let lp1 ((i 0))
            (if (fix:< i nrows)
              (let ((thesum (sigma (lambda (j) (* (matrix-ref L i j)
                                                  (matrix-ref result j k)))
                                   0 (fix:- i 1)))
                    (bi (matrix-ref b i k))
                    (lii (matrix-ref L i i)))
                (matrix-set! result i k (/ (- bi thesum) lii))
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
            (let ((thesum (sigma (lambda (j) (* (matrix-ref LT i j)
                                                (matrix-ref result j k)))
                                 (fix:+ i 1) (fix:- nrows 1)))
                  (bi (matrix-ref b i k))
                  (lii (matrix-ref LT i i)))
              (matrix-set! result i k (/ (- bi thesum) lii))
              (if (fix:> i 0) (lp1 (fix:- i 1))))
            (outerloop (fix:+ k 1)))))
      result)))

(define (cholesky A)
  (let* ((n (m:num-rows A))
         (L (m:zeros n n)))
    (let ilp ((i 0))
      (if (fix:< i n)
        (begin
          (let ((thesum (sigma (lambda (k) (* (matrix-ref L i k)
                                              (matrix-ref L i k)))
                               0 (fix:- i 1)))
                (Aii (matrix-ref A i i)))
            (matrix-set! L i i (real-part (sqrt (- Aii thesum))))) ;; getting real-part is numerical hack

          (let jlp ((j (fix:+ i 1)))
            (if (fix:< j n)
              (let ((thesum (sigma (lambda (k) (* (matrix-ref L i k)
                                                  (matrix-ref L j k)))
                                   0 (fix:- i 1)))
                    (Aij (matrix-ref A i j))
                    (Lii (matrix-ref L i i)))
                (matrix-set! L j i (/ (- Aij thesum) Lii))
                (jlp (fix:+ j 1)))))

          (ilp (fix:+ i 1)))))
    L))

(define (LDLT A)
  (let* ((n (m:num-rows A))
         (L (m:empty n n))
         (D (m:empty n 1)))
    (let ilp ((i 0))
      (if (fix:< i n)
        (begin
          (let ((thesum (sigma (lambda (k) (* (square (matrix-ref L i k))
                                              (matrix-ref D k 0)))
                               0 (fix:- i 1)))
                (Aii (matrix-ref A i i)))
            (matrix-set! D i 0 (- Aii thesum))
            (matrix-set! L i i 1))

          (let jlp ((j (fix:+ i 1)))
            (if (fix:< j n)
              (let ((thesum (sigma (lambda (k) (* (matrix-ref L i k)
                                                  (matrix-ref L j k)
                                                  (matrix-ref D k 0)))
                                   0 (fix:- i 1)))
                    (Aij (matrix-ref A i j)))
                (matrix-set! L j i (/ (- Aij thesum) (matrix-ref D i 0)))
                (jlp (fix:+ j 1)))))

          (ilp (fix:+ i 1)))))
    (cons L D)))

