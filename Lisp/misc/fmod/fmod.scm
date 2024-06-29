;; Calculate remainder of X by Y. Supports floating point, but only positive
;; values.
(define (fmod x y)
  (if (< x y)
      x
      (fmod (- x y) y)))

;; Calculate remainder of X by Y, supporting floating point and negative values.
(define (fmod x y)
  ;; Calculate remainder of X by Y, as long as they are both positive.
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

;; Calculate remainder of X by Y, supporting floating point and negative values.
(define (fmod x y)
  ;; Equivalent function to fmod() from C's math.h
  (define (c-fmod x y)
    (let ((abs-x (abs x))
          (abs-y (abs y)))
      (if (< abs-x abs-y)
          x
          (let ((abs-result (c-fmod (- abs-x abs-y) abs-y)))
            (if (< x 0)
                (- abs-result)
                abs-result)))))

  (let ((result (c-fmod x y)))
    (if (or (and (< y 0) (> result 0))
            (and (>= y 0) (< result 0)))
        (+ result y)
        result)))

;; Calculate remainder of X by Y, supporting floating point and negative values.
(define (fmod x y)
  ;; Calculate remainder of X by Y, as long as they are both positive.
  (define (fmod-positive x y)
    (if (< x y)
        x
        (fmod-positive (- x y) y)))

  (let ((abs-result (fmod-positive (abs x) (abs y))))
    (cond ((and (positive? x) (positive? y))
           abs-result)
          ((and (positive? x) (negative? y))
           (+ y abs-result))
          ((and (negative? x) (positive? y))
           (- y abs-result))
          ((and (negative? x) (negative? y))
           (- abs-result)))))

(list
 (fmod 9 4)
 (fmod 9 -4)
 (fmod -9 4)
 (fmod -9 -4)
 (fmod 9.5 2.5)
 (fmod 9.5 -2.5)
 (fmod -9.5 2.5)
 (fmod -9.5 -2.5))
