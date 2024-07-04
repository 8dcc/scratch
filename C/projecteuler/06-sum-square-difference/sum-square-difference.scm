(define (sum-squares limit)
  (if (zero? limit)
      0
      (+ (expt limit 2)
         (sum-squares (- limit 1)))))

(define (square-sum limit)
  (define (sum n)
    (if (zero? n)
        0
        (+ n (sum (- n 1)))))
  (expt (sum limit) 2))

(define (calculate-difference limit)
  (- (square-sum limit)
     (sum-squares limit)))

(calculate-difference 100)
