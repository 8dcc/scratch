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

;; The sum of `two' and `three':
;;
;;   (lambda (f) (lambda (x) (f (f x))))
;;   (lambda (f) (lambda (x) (f (f (f x)))))
;;
;; Should return `five':
;;
;;   (lambda (f) (lambda (x) (f (f (f (f (f x)))))))
;;
;; The `add' function uses the inner-most body of `b' (e.g. "(f (f (f x)))" for
;; `three') as the `x' argument of the inner lambda of `a'.
(defun add (a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; -----------------------------------------------------------------------------
;; Examples

;; Simple functions for testing church numerals.
(defun increase (val)
  (+ val 1))
(defun square (val)
  (* val val))

;; Sum of church numerals `one' and `two', that is, a function that applies a
;; function to a value 3 times.
(define church-three (add one two))
(write-to-str church-three)

;; The expression (church-three increase) returns a function that receives a
;; value `x' and that calls `increase' on it 3 times:
;;
;;   (increase (increase (increase x)))
;;
;; That function then receives 10 as the argument of `x', returning 13.
((church-three increase) 10)

;; Equivalent to:
;;
;;   (square (square (square x)))
;;
;; Which is also equivalent to:
;;
;;   ((x^2)^2)^2 = x^(2*2*2) = x^8
;;
;; In this example:
;;
;;  3^8 = 6561
((church-three square) 3)
