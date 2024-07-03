(define (valid-num? x)
  (or (zero? (modulo x 3))
      (zero? (modulo x 5))))

(define (calculate-multiples n)
  (cond ((zero? n)      0)
        ((valid-num? n) (+ n (calculate-multiples (- n 1))))
        (else           (calculate-multiples (- n 1)))))

(calculate-multiples 999)
