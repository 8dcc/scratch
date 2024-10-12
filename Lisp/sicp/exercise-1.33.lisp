
;; Exercise 1.33
;; =============
;;
;; You can obtain an even more general version of `accumulate' (Exercise 1.32)
;; by introducing the notion of a /filter/ on the terms to be combined. That is,
;; combine only those terms derived from values in the range that satisfy a
;; specified condition. The resulting `filtered-accumulate' abstraction takes
;; the same arguments as `accumulate', together with an additional predicate of
;; one argument that specifies the filter. Write `filtered-accumulate' as a
;; procedure. Show how to express the following using `filtered-accumulate':
;;
;; a. The sum of the squares of the prime numbers in the interval `a' to `b'
;;    (assuming that you have a `prime?' predicate already written).
;; b. The product of all the positive integers less than `n' that are relatively
;;    prime to `n' (i.e., all positive integers `i < n' such that
;;    `GCD(i, n) = 1').
;;
;; Notes
;; -----
;;
;; At first, I thought the `filter' was supposed to substitute `>' in the
;; `accumulate' version from the previous exercise, but it's meant to be used as
;; a way of ignoring some values of `a'.
;;
;; Alternative implementation
;; --------------------------
;;
;; A function like `filtered-accumulate' is often implemented using the
;; following process:
;;
;;   1. A function makes a sequence of integers.
;;   2. A `filter' function that filters the sequence using the specified
;;      filtering function.
;;   3. The filtered list is transformed with `map', so that the specified
;;      `term' is applied to each element.
;;   4. The list is accumulated using `reduce'.
;;
;; The usual reduction process consists on progressively applying the specified
;; procedure (in our version `combiner'), to a stored result (which starts as
;; the first element of the list) and the next element in the list. Therefore:
;;
;;   (reduce list '(1 2 3 4))  =>  (((1 2) 3) 4)
;;
;; Note that the Scheme version of `reduce' takes an extra `initial' argument
;; (in our version `null-value'), but it's only used if the list is empty.
;;
;; Therefore, we could define `filtered-accumulate' as:
;;
;;   (defun filtered-accumulate (combiner null-value term a next b fltr)
;;     (reduce combiner
;;             null-value
;;             (map term
;;                  (filter fltr
;;                          (make-integer-sequence-by-step a next b)))))
;;
;; Credits for this explanation to TMA in #lisp

(defun filtered-accumulate (combiner null-value term a next b filter)
  (defun iter (a result)
    (if (> a b)
        result
        (iter (next a)
              (if (filter a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))

;;------------------------------------------------------------------------------

(defun prime? (n)
  (defun square (x)
    (* x x))
  (defun divisible? (a b)
    (= 0 (mod a b)))
  (defun smallest-divisor (n test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divisible? n test-divisor)
           test-divisor)
          (tru
           (smallest-divisor n (+ 1 test-divisor)))))
  (= n (smallest-divisor n 2)))

(defun sum-squared-primes (a b)
  (filtered-accumulate +                    ; Sum
                       0                    ; Base result is zero
                       (lambda (x) (* x x)) ; Square each processed number
                       a                    ; From `a'
                       (lambda (x) (+ x 1)) ; Each iteration, increase `a'
                       b                    ; To `b'
                       prime?))             ; Only accumulate prime numbers

;; 1^2 + 2^2 + 3^2 + 5^2 + 7^2 = 88
(sum-squared-primes 1 10)

;;------------------------------------------------------------------------------

(defun gcd (a b)
  (cond ((= a 0) b)
        ((= a b) a)
        ((> a b) (gcd (mod a b) b))
        (tru (gcd b a))))

(defun product-primes (n)
  (filtered-accumulate *                              ; Product
                       1                              ; Base result is one
                       (lambda (x) x)                 ; Each number, unchanged
                       1                              ; From `1'
                       (lambda (x) (+ x 1))           ; Increase by one
                       n                              ; To `n'
                       (lambda (x) (= 1 (gcd x n))))) ; Only relative primes

(mapcar (lambda (x) (list x (gcd x 10)))
        '(1 2 3 4 5 6 7 8 9 10))

;; 1 * 3 * 7 * 9 = 189
(product-primes 10)
