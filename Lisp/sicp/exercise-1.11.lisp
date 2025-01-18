
;; Exercise 1.11
;; =============
;;
;; A function `f' is defined by the rule that:
;;
;;   f(n) = n                           if n < 3
;;   f(n) = f(n-1) + 2f(n-2) + 3f(n-3)  if n >= 3
;;
;; Write a procedure that computes `f' by means of a recursive process. Write a
;; procedure that computes `f' by means of an iterative process.
;;
;;------------------------------------------------------------------------------
;;
;; The recursive version is pretty straight forward. Basically a 1:1 translation
;; of the mathematical formula.

(defun f-recur (n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))

(f-recur 2)  ; Expected: 2
(f-recur 5)  ; Expected: 25

;;------------------------------------------------------------------------------
;;
;; The iterative version is more tricky to understand. First, we need to check
;; if we need to perform iterations or not (n>3). When performing the
;; iterations, we have to think/iterate from the base case to the more complex
;; cases, since they depend on the base ones. In other words, if you look at
;; Figure 1.5 of SICP, the idea is to start by calculating the lower nodes of
;; the tree, and re-use/accumulate those values for the upper nodes.
;;
;; The counter `i' corresponds to the parameter `n' in the recursive version. In
;; other words, if `i' is 7, it means that we are trying to calculate `f(7)' in
;; the current iteration. The `result1', `result2' and `result3' parameters of
;; `iter' will hold what would be the result of calling `f(n-1)', `f(n-2)' and
;; `f(n-3)' recursively. Each iteration, a new result is calculated by adding
;; the 3 previous results we receive as parameters. This new result is used as
;; the second parameter (`result1') for the next iteration, and the other two
;; are shifted. Logically, `result3' is discarded each iteration.
;;
;; For example, when calculating `f(5)', the `f-iter' function will call `iter'
;; with the index 2, assuming the previous results were 2, 1 and 0. On the first
;; call/iteration, `iter' will calculate `f(3)'. Once it's calculated, the
;; results for the next iteration become 4, 2 and 1; where 4 is the result of
;; `f(3)'. On the second iteration, `f(4)' will be calculated using those
;; values. This cycle continues until the counter `i' reaches 6, at that point
;; `result1' contains the value of `f(5)', so it's returned.
;;
;; NOTE: The inner `iter' function is able to access the parameter `n' of the
;; outer function, even though currently (commit b9e1c8a) SL doesn't support
;; closures. This is because when calling `iter', the `n' variable is still
;; bound.

(defun f-iter (n)
  (defun iter (i result1 result2 result3)
    (if (> i n)
        result1
        (iter (+ i 1)
              (+ result1 (* 2 result2) (* 3 result3))
              result1
              result2)))
  (if (< n 3)
      n
      (iter 3 2 1 0)))

(f-iter 2)  ; Expected: 2
(f-iter 5)  ; Expected: 25
