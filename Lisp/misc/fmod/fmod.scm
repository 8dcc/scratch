;; Calculate X mod Y, supporting floating point. X and Y must be positive.
(define (fmod-positive x y)
  (if (< x y)
      x
      (fmod-positive (- x y) y)))

;; Calculate X mod Y, supporting floating point
(define (fmod x y)
  (define (fmod-positive x y)
    (if (< x y)
        x
        (fmod-positive (- x y) y)))

  ;; If X is negative: X mod Y = Y - (|X| mod Y)
  (define (fmod-y-positive x y)
    (if (< x 0)
        (- y (fmod-positive (- x) y))
        (fmod-positive x y)))

  ;; If X is negative: X mod Y = -(-X mod -Y)
  (if (< y 0)
      (- (fmod-y-positive (- x) (- y)))
      (fmod-y-positive x y)))

(list
 (fmod 9 4)
 (fmod -9 4)
 (fmod 9 -4)
 (fmod -9 -4)
 (fmod 9.5 2.5)
 (fmod -9.5 2.5)
 (fmod 9.5 -2.5)
 (fmod -9.5 -2.5))
