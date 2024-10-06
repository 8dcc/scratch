
;; Exercise 1.31
;; =============
;;
;; The sum procedure is only the simplest of a vast number of similar
;; abstractions that can be captured as higher-order procedures. Write an
;; analogous procedure called product that returns the product of the values of
;; a function at points over a given range. Show how to define `factorial' in
;; terms of product. Also use product to compute approximations to PI using the
;; formula:
;;
;;   PI     2 * 4 * 4 * 6 * 6 * 8 * ...
;;  ---- = -----------------------------
;;   4      3 * 3 * 5 * 5 * 7 * 7 * ...
;;
;; If your `product' procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write
;; one that generates a recursive process

(defun product-recur (term a next b)
  (if (> a b)
      1.0
      (* (term a)
         (product-recur term (next a) next b))))

(defun product-iter (term a next b)
  (defun iter (a result)
    (if (> a b)
        result
        (iter (next a)
              (* result (term a)))))
  (iter a 1.0))

;;------------------------------------------------------------------------------

;; The factorial of a number is just the product of the numbers themselves, from
;; 1 to N, increasing one by one.
(defun fact (n)
  (product-iter (lambda (x) x)
                1
                (lambda (x) (+ x 1))
                n))

(fact 5) ; Expected: 120

;;------------------------------------------------------------------------------

(defun wallis-product (accuracy)
  ;; `product-pi' calculates the top or bottom part of the division, starting at
  ;; the argument START and ending at ACCURACY.
  (defun product-pi (start)
    (product-iter (lambda (x) (* x x))
                  start
                  (lambda (x) (+ x 2))
                  accuracy))
  (/ (* 2 (product-pi 4))
     (* (product-pi 3) (+ accuracy 1))))

(defun approximate-pi (accuracy)
  (* 4 (wallis-product accuracy)))

(approximate-pi 100) ; Expected: ~3.12

;;------------------------------------------------------------------------------

;; This version is much better because it uses the alternative formula:
;;
;;    PI                      2i         2i
;;   ---- = prod_(i=1)^n ( -------- * -------- )
;;    2                     2i - 1     2i + 1
;;
;;
(defun wallis-product-alternative (n)
  (defun term (i)
    (* (/ (* 2 i)
          (- (* 2 i) 1))
       (/ (* 2 i)
          (+ (* 2 i) 1))))
  (product-iter term
                1
                (lambda (x) (+ x 1))
                n))

(defun approximate-pi-alternative (accuracy)
  (* 2 (wallis-product-alternative accuracy)))

(approximate-pi-alternative 100) ; Expected: ~3.13
