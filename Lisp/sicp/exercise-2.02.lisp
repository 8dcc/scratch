
;; Exercise 2.2
;; ============
;;
;; Consider the problem of representing line segments in a plane. Each segment
;; is represented as a pair of points: a starting point and an ending
;; point. Define a constructor `make-segment' and selectors `start-segment' and
;; `end-segment' that define the representation of segments in terms of
;; points. Furthermore, a point can be represented as a pair of numbers: the X
;; coordinate and the Y coordinate. Accordingly, specify a constructor
;; `make-point' and selectors `x-point' and `y-point' that define this
;; representation. Finally, using your selectors and constructors, define a
;; procedure `midpoint-segment' that takes a line segment as argument and
;; returns its midpoint (the point whose coordinates are the average of the
;; coordinates of the endpoints).

;; -----------------------------------------------------------------------------
;; Dependencies

(defun average (a b)
  (/ (+ a b) 2))

;; -----------------------------------------------------------------------------
;; Exercise

(defun make-point (x y)
  (assert (and (flt? x) (flt? y)))
  (cons x y))
(defun x-point (p)
  (car p))
(defun y-point (p)
  (cdr p))

(defun make-segment (start end)
  (cons start end))
(defun start-segment (s)
  (car s))
(defun end-segment (s)
  (cdr s))

(defun midpoint-segment (s)
  (let ((start (start-segment s))
        (end   (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

;; -----------------------------------------------------------------------------
;; Examples

(defun format-point (p)
  (format "(%f,%f)" (x-point p) (y-point p)))

(defun format-segment (s)
  (format "%s-%s"
          (format-point (start-segment s))
          (format-point (end-segment s))))

(let ((segment (make-segment (make-point 2.0 2.0)
                             (make-point 4.0 5.0))))
  (format "%s => %s"
          (format-segment segment)
          (format-point (midpoint-segment segment))))
