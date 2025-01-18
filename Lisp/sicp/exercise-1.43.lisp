
;; Exercise 1.43
;; =============
;;
;; If `f' is a numerical function and `n' is a positive integer, then we can
;; form the n-th repeated application of `f', which is defined to be the
;; function whose value at `x' is
;;
;;   f(f(...(f(x))...))
;;
;; For example, if `f' is the function
;;
;;   x |-> x + 1
;;
;; then the nth repeated application of `f' is the function
;;
;;   x |-> x + n
;;
;; If `f' is the operation of squaring a number, then the n-th repeated
;; application of `f' is the function that raises its argument to the (2^n)-th
;; power. Write a procedure that takes as inputs a procedure that computes `f'
;; and a positive integer `n' and returns the procedure that computes the n-th
;; repeated application of `f'. Your procedure should be able to be used as
;; follows:
;;
;;   ((repeated square 2) 5)
;;     => 625
;;
;; Hint: You may find it convenient to use compose from Exercise 1.42.

(defun compose (f g)
  (lambda (arg)
    (f (g arg))))

(defun repeated (f times)
  (if (<= times 1)
      f
      (compose f (repeated f (- times 1)))))

(defun inc (n)
  (+ n 1))
(defun square (n)
  (* n n))

((repeated inc 6) 5)     ; Expected: 11
((repeated square 2) 5)  ; Expected: 625
