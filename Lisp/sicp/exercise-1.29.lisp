
;; Exercise 1.29
;; =============
;;
;; Simpson's Rule is a more accurate method of numerical integration than the
;; method illustrated above.  Using Simpson's Rule, the integral of a function
;; `f' between `a' and `b' is approximated as
;;
;;   h/3 * (y0 + 4*y1 + 2*y2 + 4*y3 + 2*y4 + ... + 2*yn-2 + 4*yn-1 + yn)
;;
;; where `h = (b - a)/n', for some even integer `n', and `yk = f (a + kh)'.
;; (Increasing `n' increases the accuracy of the approximation.) Define a
;; procedure that takes as arguments `f', `a', `b', and `n' and returns the
;; value of the integral, computed using Simpson's Rule. Use your procedure to
;; integrate cube between 0 and 1 (with `n' = 100 and `n' = 1000), and compare
;; the results to those of the integral procedure shown above.
;;
;; Extra Emacs stuff:
;; ------------------
;;
;; SL to Scheme:
;; (query-replace-regexp "defun \\(.+\\) (\\(.+\\))" "define (\\1 \\2)")
;; (query-replace "(mod " "(modulo ")
;; (query-replace-regexp "\\_<tru\\_>" "#t")
;;
;; Scheme to SL:
;; (query-replace-regexp "define (\\(.+?\\)\\(\s+\\(.*\\)\\)?)" "defun \\1 (\\3)")
;; (query-replace "(modulo " "(mod ")
;; (query-replace-regexp "#t\\_>" "tru")

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(defun even? (n)
  (= (mod n 2.0) 0.0))

(defun simpsons-integral (f a b n)
  (let ((h (/ (- b a) n)))
    (defun next (k)
      (+ 1 k))
    (defun term (k)
      (let ((factor (cond ((or (= k 0) (= k n)) 1)
                          ((even? k)            2)
                          (tru                  4)))
            (y (f (+ a (* k h)))))
        (* factor y)))
    (* (/ h 3)
       (sum term 0 next n))))

(defun cube (n)
  (* n n n))

(simpsons-integral cube 0 1 100)  ; Result: 0.250000
(simpsons-integral cube 0 1 1000) ; Result: 0.250000
