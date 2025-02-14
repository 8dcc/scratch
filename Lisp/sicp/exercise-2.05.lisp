
;; Exercise 2.5
;; ============
;;
;; Show that we can represent pairs of nonnegative integers using only numbers
;; and arithmetic operations if we represent the pair 'a' and 'b' as the integer
;; that is the product 2^a*3^b. Give the corresponding definitions of the
;; procedures `cons', `car', and `cdr'.
;;
;; Notes
;; -----
;;
;; The bases used in the exponentiation don't have to be 2 and 3, they just need
;; to be primitive numbers.
;;
;; With this method, we are encoding the value of 'a' and 'b' as the number of
;; times that 2 or 3 were multiplied by themselves, respectively.
;;
;;     2^4 * 2^5 = 2 * 2 * 2 * 2 * 3 * 3 * 3 * 3 *3 = 3888
;;
;; We can repeatedly divide the result of the multiplication by 2, and count the
;; number of divisions until a remainder is encountered; that number will be the
;; first exponent, i.e. our `car'. The same can be done with 3 to obtain the
;; `cdr'.

;; -----------------------------------------------------------------------------
;; Dependencies

(defun expt (b e)
  (defun iter (total e)
    (cond ((= e 0) 1)
          ((= e 1) total)
          (tru (iter (* total b) (- e 1)))))
  (iter b e))

(defun zero? (n)
  (= n 0))

;; -----------------------------------------------------------------------------
;; Exercise

(define car-base 2)
(define cdr-base 3)

(defun my-cons (a b)
  (* (expt car-base a)
     (expt cdr-base b)))

;; Given a BASE and TOTAL, return an exponent such that: BASE ^ result = TOTAL
(defun obtain-exponent (base total)
  (defun iter (total i)
    (if (zero? (mod total base))
        (iter (/ total car-base) (+ 1 i))
        i))
  (iter total 0))

(defun my-car (n)
  (obtain-exponent car-base n))

(defun my-cdr (n)
  (obtain-exponent cdr-base n))

;; -----------------------------------------------------------------------------
;; Examples

(my-cons 4 5)          ; Expected: 3888
(my-car (my-cons 4 5)) ; Expected: 4
(my-cdr (my-cons 4 5)) ; Expected: 5
