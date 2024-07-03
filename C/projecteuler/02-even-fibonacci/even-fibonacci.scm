(define (calculate limit)
  (define (do-calculation cur-fib prev-fib limit)
    (cond ((>= cur-fib limit)
           0)
          ((even? cur-fib)
           (+ cur-fib
              (do-calculation (+ cur-fib prev-fib) cur-fib limit)))
          (else
           (do-calculation (+ cur-fib prev-fib) cur-fib limit))))
  (do-calculation 1 1 limit))

(calculate 4000000)
