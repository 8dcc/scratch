
;; Exercise 1.16
;; =============
;;
;; Design a procedure that evolves an iterative exponentiation process that uses
;; successive squaring and uses a logarithmic number of steps, as does
;; fast-expt. (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep,
;; along with the exponent `n' and the base `b', an additional state variable
;; `a', and define the state transformation in such a way that the product ab^n
;; is unchanged from state to state. At the beginning of the process a is taken
;; to be 1, and the answer is given by the value of a at the end of the process.
;; In general, the technique of defining an invariant quantity that remains
;; unchanged from state to state is a powerful way to think about the design of
;; iterative algorithms.)
;;
;; Notes
;; -----
;;
;; The problem mentions that, in general, it's useful to "define an invariant
;; quantity that remains unchanged from state to state"; and, in this case, "the
;; product ab^n [should remain] unchanged from state to state". What it means by
;; this is that, in each iteration, we should increment/decrement the
;; accumulator `a', the base `b' or the exponent `n' in a way that doesn't
;; affect the product of ab^n. This was a bit confusing for me before trying the
;; actual problem, but it became more obvious once I reviewed my solution.
;;
;; One important detail for understanding this iterative version is that any
;; even exponent will have to go through an iteration where `n' is 2, and then
;; another where `n' is one. For example:
;;
;;   - N starts at 6
;;     - It's even, gets divided by two. N is 3.
;;     - It's odd, one is subtracted. N is 2.
;;     - It's even, gets divided by two. N is 1.
;;     - It's odd, one is subtracted. N is 0.
;;     - It's zero. Accumulator is returned.
;;   - N starts at 8
;;     - It's even, gets divided by two. N is 4.
;;     - It's even, gets divided by two. N is 2.
;;     - It's even, gets divided by two. N is 1.
;;     - It's odd, one is subtracted. N is 0.
;;     - It's zero. Accumulator is returned.
;;
;; Therefore, the process will always have a call where `n' is 1, and they will
;; have to enter to the odd `cond' clause. This is important since the
;; accumulator `a' is what is ultimately returned, but it will not be modified
;; in the even clause.
;;
;; On even clauses, the base is squared and the exponent is divided by two. On
;; odd clauses, the accumulator is multiplied by the base and the exponen is
;; reduced by one, resulting in an even/zero result for the next iteration.

(defun fast-expt (b n)
  (defun even? (n)
    (equal? (mod n 2) 0))
  (defun square (n)
    (* n n))
  (defun iter (a b n)
    (cond ((equal? n 0) a)
          ((even? n) (iter a (square b) (/ n 2))) ; b = b^2; n /= 2;
          (else (iter (* a b) b (- n 1)))))       ; a *= b^(n-1)
  (iter 1 b n))

(fast-expt 5 7)  ; Expected: 78125
(fast-expt 5 8)  ; Expected: 390625
