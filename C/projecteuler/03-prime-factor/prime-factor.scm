;; Returns #t if ELEMENT is in LIST. Returns #f otherwise.
(define (member? element list)
  (cond ((null? list) #f)
        ((equal? element (car list)) #t)
        (else (member? element (cdr list)))))

;; Returns prime factors of TARGET in descending order.
(define (prime-factors target)
  (define (prime-factors-recur target i list)
    (cond ((> i target)
           list)
          ((and (not (member? i list))
                (zero? (modulo target i)))
           (prime-factors-recur (/ target i)
                                (+ i 1)
                                (cons i list)))
          (else
           (prime-factors-recur target (+ i 1) list))))
  (prime-factors-recur target 2 '()))

(car (prime-factors 600851475143))
