
;; Exercise 1.32
;; =============
;;
;; Show that `sum' and `product' (Exercise 1.31) are both special cases of a
;; still more general notion called `accumulate' that combines a collection of
;; terms, using some general accumulation function:
;;
;;   (accumulate combiner null-value term a next b)
;;
;; `accumulate' takes as arguments the same term and range specifications as
;; `sum' and `product', together with a `combiner' procedure (of two arguments)
;; that specifies how the current term is to be combined with the accumulation
;; of the preceding terms and a `null-value' that specifies what base value to
;; use when the terms run out. Write `accumulate' and show how `sum' and
;; `product' can both be defined as simple calls to `accumulate'.
;;
;; If your `accumulate' procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write
;; one that generates a recursive process.

(defun accumulate-recur (combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner
                                  null-value
                                  term
                                  (next a)
                                  next
                                  b))))

(defun accumulate-iter (combiner null-value term a next b)
  (defun iter (a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))

;;------------------------------------------------------------------------------

(defun sum (term a next b)
  (accumulate-iter + 0 term a next b))

(defun product (term a next b)
  (accumulate-iter * 1 term a next b))

;;------------------------------------------------------------------------------

;; sum_(i=5)^10 i^2 = 355
(sum (lambda (x) (* x x))
     5
     (lambda (x) (+ x 1))
     10)

;; prod_(i=5)^10 i/2 = 2362.5
(product (lambda (x) (/ x 2))
         5
         (lambda (x) (+ x 1))
         10)
