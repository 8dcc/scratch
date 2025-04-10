
;; Exercise 2.4
;; ============
;;
;; Here is an alternative procedural representation of pairs. For this
;; representation, verify that '(car (cons x y))' yields 'x' for any objects 'x'
;; and 'y'.
;;
;;   (define (cons x y)
;;     (lambda (m) (m x y)))
;;
;;   (define (car z)
;;     (z (lambda (p q) p)))
;;
;; What is the corresponding definition of `cdr'? (Hint: To verify that this
;; works, make use of the substitution model of Section 1.1.5.)

(defun my-cons (x y)
  (lambda (m) (m x y)))

(defun my-car (z)
  (z (lambda (p q) p)))

(defun my-cdr (z)
  (z (lambda (p q) q)))
