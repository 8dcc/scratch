;; Exercise 2.6
;; ============
;;
;; In case representing pairs as procedures wasn't mind-boggling enough,
;; consider that, in a language that can manipulate procedures, we can get by
;; without numbers (at least insofar as nonnegative integers are concerned) by
;; implementing 0 and the operation of adding 1 as:
;;
;;   (define zero (lambda (f) (lambda (x) x)))
;;   (define (add-1 n)
;;     (lambda (f) (lambda (x) (f ((n f) x)))))
;;
;; This representation is known as Church numerals, after its inventor, Alonzo
;; Church, the logician who invented the lambda-calculus.
;;
;; Define `one' and `two' directly (not in terms of `zero' and `add-1'). (Hint:
;; Use substitution to evaluate '(add-1 zero)'). Give a direct definition of the
;; addition procedure `+' (not in terms of repeated application of `add-1').
;;
;; Notes
;; -----
;;
;; The reader might find interesting my article about the Y-combinator, which
;; explains some lambda-calculus concepts:
;;   https://8dcc.github.io/programming/understanding-y-combinator.html

;; -----------------------------------------------------------------------------
;; Dependencies

;; The symbol `zero' represents a Church numeral with a value of 0.
;;
;; It is bound to a function that receives an unused argument `f', and returns
;; another function that receives an argument `x' and returns it unchanged
;; (identity function).
(define zero
  (lambda (f) (lambda (x) x)))

;; The `add-1' function receives a Church numeral `n' (i.e. a function with a
;; similar structure as `zero'), and returns the next church numeral.
(defun add-1 (n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; -----------------------------------------------------------------------------
;; Exercise

;; The symbol `one' represents a Church numeral with a value of 1.
;;
;; This can be obtained from substituting the argument `zero' in a call to
;; `add-1' (i.e. through beta-reduction). These are the steps that were used:
;;
;; 1. Expand the call to `add-1'.
;;
;;     (add-1                                                zero)
;;      ^^^^^                                                ^^^^
;;      vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv vvvvvvvvvvvvvvvvvvvvvvvvvvv
;;     ((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (f) (lambda (x) x)))
;;
;; 2. Evaluate outer-most call, replacing `zero' as the N parameter in the body
;;    of the function. Rename the F parameter of `zero' to Y, and the X from the
;;    body of `zero' to Z.
;;
;;     ((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (f) (lambda (x) x)))
;;                                               ^
;;                                   vvvvvvvvvvvvvvvvvvvvvvvvvvv
;;      (lambda (f) (lambda (x) (f (((lambda (y) (lambda (z) z)) f) x))))
;;
;; 3. Simplify the lambda by evaluating the call to `zero', that is, the call to
;;    the lambda whose parameter is Y and whose argument is F.
;;
;;     (lambda (f) (lambda (x) (f (((lambda (y) (lambda (z) z)) f) x))))
;;                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                                 vvvvvvvvvvvvvv
;;     (lambda (f) (lambda (x) (f ((lambda (z) z) x))))
;;
;; 4. Simplify the call to the identity lambda, that is, the call to the lambda
;;    whose parameter is Z and whose argument is X.
;;
;;     (lambda (f) (lambda (x) (f ((lambda (z) z) x))))
;;                                ^^^^^^^^^^^^^^^^^^
;;                                v
;;     (lambda (f) (lambda (x) (f x)))
;;
;; That is the final simplified result.
(define one
  (lambda (f) (lambda (x) (f x))))

;; Similarly, this is the reduction process for calculating `two':
;;
;;   1. (add-1 one)
;;   2. ((lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))
;;       (lambda (f) (lambda (x) (f x))))
;;   3. (lambda (f) (lambda (x) (f (((lambda (y) (lambda (z) (y z))) f) x))))
;;   4. (lambda (f) (lambda (x) (f ((lambda (z) (f z)) x))))
;;   5. (lambda (f) (lambda (x) (f (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; TODO: Arbitrary addition.
