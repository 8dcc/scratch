
;; Exercise 2.1
;; ============
;;
;; Define a better version of `make-rat' that handles both positive and negative
;; arguments. The `make-rat' procedure should normalize the sign so that if the
;; rational number is positive,both the numerator and denominator are positive,
;; and if the rational number is negative, only the numerator is negative.
;;
;; Notes
;; -----
;;
;; The description of the exercise was not very clear to me, but the intended
;; behavior is:
;;
;;   1. If the denominator is positive, both numerator and denominator remain
;;      unchanged.
;;   2. If the denominator is negative, the sign of the numerator and the
;;      denominator are inverted.
;;
;; Some examples:
;;
;;   | Input | Normalized |
;;   |-------+------------|
;;   | 2/3   | 2/3        |
;;   | -2/3  | -2/3       |
;;   | 2/-3  | -2/3       |
;;   | -2/-3 | 2/3        |
;;
;; Also note that my `simplify-rat' function is the same as the `make-rat'
;; function shown in Section 2.1.1; I just chose to separate them for
;; readability. To make the actually desired function, you could use something
;; like:
;;
;;   (defun make-better-rat (n d)
;;     (normalize-rat (simplify-rat (make-rat n d))))
;;
;; I think this approach is cleaner and more SICP-like than combining all 3
;; procedures into one.

;; -----------------------------------------------------------------------------
;; Dependencies

(define make-rat cons)
(define numer car)
(define denom cdr)

(defun format-rat (x)
  (format "%f/%f" (numer x) (denom x)))

(defun abs (n)
  (if (< n 0) (- n) n))

;; From exercise 1.33
(defun gcd (a b)
  (cond ((= a 0) b)
        ((= a b) a)
        ((> a b) (gcd (mod a b) b))
        (tru (gcd b a))))

(defun simplify-rat (x)
  (let* ((n (numer x))
         (d (denom x))
         (g (gcd (abs n) (abs d))))
    (make-rat (/ n g)
              (/ d g))))

;; -----------------------------------------------------------------------------
;; Exercise

(defun normalize-rat (x)
  (let ((sign (if (< (denom x) 0) -1 1)))
    (make-rat (* sign (numer x))
              (* sign (denom x)))))

;; -----------------------------------------------------------------------------
;; Examples

(defun make-better-rat (n d)
  (normalize-rat (simplify-rat (make-rat n d))))

(format-rat (make-better-rat  4  6)) ; Expected: "2.0/3.0"
(format-rat (make-better-rat -4  6)) ; Expected: "-2.0/3.0"
(format-rat (make-better-rat  4 -6)) ; Expected: "-2.0/3.0"
(format-rat (make-better-rat -4 -6)) ; Expected: "2.0/3.0"
