
;; Exercise 2.7
;; ============
;;
;; Alyssa's program is incomplete because she has not specified the
;; implementation of the interval abstraction. Here is a definition of the
;; interval constructor:
;;
;;     (define (make-interval a b) (cons a b))
;;
;; Define selectors `upper-bound' and `lower-bound' to complete the
;; implementation.
;;

;; -----------------------------------------------------------------------------
;; Dependencies

(defun make-interval (a b)
  (cons a b))

;; -----------------------------------------------------------------------------
;; Exercise

(defun lower-bound (interval)
  (car interval))

(defun upper-bound (interval)
  (cdr interval))

;; -----------------------------------------------------------------------------
;; Examples

;; NOTE: Provided by the exercise.
(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; NOTE: Provided by the exercise.
(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Expected result: (7 . 12)
(add-interval (make-interval 3 5)
              (make-interval 4 7))

;; Expected result: (12 . 35)
(mul-interval (make-interval 3 5)
              (make-interval 4 7))
