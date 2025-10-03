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
;; The reader might find interesting my article about Church numerals, mainly
;; motivated by this exercise, which explains the main lambda-calculus concepts,
;; along with the full explanations on the reduction processes below:
;;
;;   https://8dcc.github.io/math/understanding-church-numerals.html
;;
;; See also my article about the Y-combinator
;;
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
;;   1. (add-1 zero)
;;   2. ((lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))
;;       (lambda (f) (lambda (x) x)))
;;   3. (lambda (f) (lambda (x) (f (((lambda (y) (lambda (z) z)) f) x))))
;;   4. (lambda (f) (lambda (x) (f ((lambda (z) z) x))))
;;   5. (lambda (f) (lambda (x) (f x)))
;;
;; Again, please see my article about Church numerals for a detailed explanation
;; of each step:
;;   https://8dcc.github.io/math/understanding-church-numerals.html
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
