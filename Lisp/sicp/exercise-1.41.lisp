
;; Exercise 1.41
;; =============
;;
;; Define a procedure `double' that takes a procedure of one argument as
;; argument and returns a procedure that applies the original procedure
;; twice. For example, if `inc' is a procedure that adds 1 to its argument, then
;; '(double inc)' should be a procedure that adds 2. What value is returned by
;;
;;   (((double (double double)) inc) 5)

;; FIXME: When writing this, closures in SL are not implemented, so the lambda
;; returned by `double' can't access the argument `f'.
(defun double (f)
  (lambda (arg)
    (f (f arg))))

(defun inc (n)
  (+ n 1))

((double inc) 5)  ; Expected: 5+1+1=7

; ------------------------------------------------------------------------------
;; Inner-most call returns a lambda that calls its argument 4 (2*2) times. That
;; gets doubled again to produce a function that returns a lambda that calls its
;; argument 16 (4*4) times. Finally, that lambda is applied to `inc', so the
;; final function adds 16. Note how the number of calls increases exponentially,
;; not linearly.

(((double (double double)) inc) 5)  ; Expected: 5+16=21
