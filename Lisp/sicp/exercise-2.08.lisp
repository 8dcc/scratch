
;; Exercise 2.8
;; ============
;;
;; Using reasoning analogous to Alyssa's, describe how the difference of two
;; intervals may be computed. Define a corresponding subtraction procedure,
;; called `sub-interval'.
;;
;; Notes
;; -----
;;
;; In the following example intervals:
;;
;;     -F   -A   -5    0    5    A    F
;;      v    v    v    v    v    v    v
;;      ---------------+---------------
;;                         ####          X: (4, 7)
;;                        ###            Y: (3, 5)
;;
;; The lower bound would be -1, where the value of X is the minimum (4) and the
;; value of Y is the maximum (5); and the upper bound would be 4, where value of
;; X is the maximum (7) and the value of Y is the minimum (3).

;; -----------------------------------------------------------------------------
;; Dependencies

(defun make-interval (a b)
  (cons a b))

(defun lower-bound (interval)
  (car interval))

(defun upper-bound (interval)
  (cdr interval))

;; -----------------------------------------------------------------------------
;; Exercise

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; -----------------------------------------------------------------------------
;; Examples

;; Expected result: (-1 . 4)
(sub-interval (make-interval 4 7)
              (make-interval 3 5))
