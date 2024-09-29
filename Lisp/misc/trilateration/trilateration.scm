(define (square n)
  (* n n))

(define (distance a b)
  (sqrt (+ (square (- (car b) (car a)))
           (square (- (cdr b) (cdr a))))))

(distance '(1 . 2) '(5 . 1))

(define (trilaterate-positive s1 d1 s2 d2)
  (define (calculate-px)
    (let ((station-distance (distance s1 s2)))
      (/ (+ (- (square d1) (square d2))
            (square station-distance))
         (* 2 station-distance))))
  (define (calculate-py px)
    (sqrt (- (square d1)
             (square px))))
  (let ((px (calculate-px)))
    (cons px (calculate-py px))))

(define (trilaterate s1 d1 s2 d2)
  (let ((result (trilaterate-positive s1 d1 s2 d2)))
    (list result
          (cons (car result)
                (- (cdr result))))))

(trilaterate '(1 . 2) 2
             '(5 . 1) 3)
