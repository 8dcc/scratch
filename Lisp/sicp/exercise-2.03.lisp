
;; Exercise 2.3
;; ============
;;
;; Implement a representation for rectangles in a plane. (Hint: You may want to
;; make use of Exercise 2.2.) In terms of your constructors and selectors,
;; create procedures that compute the perimeter and the area of a given
;; rectangle. Now implement a different representation for rectangles. Can you
;; design your system with suitable abstraction barriers, so that the same
;; perimeter and area procedures will work using either representation?
;;
;; Notes
;; -----
;;
;; We start by defining the final `rect-area' and `rect-perimeter' functions,
;; calling some selectors that we haven't yet defined (wishful thinking).
;;
;; Then, we define different constructors and selectors that use different kinds
;; of internal representations. The important part is that the final area and
;; perimeter functions just trust that they receive some object that represents
;; a rectangle, and that it's a valid input for some other functions that
;; retreive data from the rectangle.

;; -----------------------------------------------------------------------------
;; Dependencies

(defun average (a b)
  (/ (+ a b) 2))

;; From exercise 2.2
(defun make-point (x y)
  (assert (and (flt? x) (flt? y)))
  (cons x y))
(defun x-point (p)
  (car p))
(defun y-point (p)
  (cdr p))

;; -----------------------------------------------------------------------------
;; Exercise

(defun rect-area (r)
  (* (rect-width r) (rect-height r)))
(defun rect-perimeter (r)
  (* 2 (+ (rect-width r) (rect-height r))))

;; First implementation, the minimum information that the area and perimeter
;; functions need.
(defun impl1-make-rect (width height)
  (cons width height))
(defun impl1-rect-width (r)
  (car r))
(defun impl1-rect-height (r)
  (cdr r))

;; Second implementation, more internal details which are unused by the area and
;; perimeter functions.
(defun impl2-make-rect (start-point end-point)
  (cons start-point end-point))
(defun impl2-rect-start (r)
  (car r))
(defun impl2-rect-end (r)
  (cdr r))
(defun impl2-rect-width (r)
  (- (x-point (impl2-rect-end r))
     (x-point (impl2-rect-start r))))
(defun impl2-rect-height (r)
  (- (y-point (impl2-rect-end r))
     (y-point (impl2-rect-start r))))

;; -----------------------------------------------------------------------------
;; Examples

(let ((rect-width impl1-rect-width)
      (rect-height impl1-rect-height)
      (my-rect (impl1-make-rect 5 7)))
  (list (rect-area my-rect)
        (rect-perimeter my-rect)))

(let ((rect-width impl2-rect-width)
      (rect-height impl2-rect-height)
      (my-rect (impl2-make-rect (make-point 3.0 3.0)
                                (make-point 8.0 10.0))))
  (list (rect-area my-rect)
        (rect-perimeter my-rect)))
