
;; Exercise 1.18
;; =============
;;
;; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that
;; generates an iterative process for multiplying two integers in terms of
;; adding, doubling, and halving and uses a logarithmic number of steps.
;;
;; Notes
;; -----
;;
;; I did this exercise before reading it, on commit fa66c46.
;; See exercise-1.17.lisp for more comments.

(defun even? (n)
  (equal? (mod n 2) 0))
(defun double (n)
  (* n 2))
(defun halve (n)
  (/ n 2))

(defun fast-mul (a b)
  (defun iter (total a b)
    (cond ((equal? b 0) total)
          ((even? b) (iter total (double a) (halve b))) ; a *= 2; b /= 2;
          (else (iter (+ total a) a (- b 1)))))         ; total += a; b -= 1;
  (iter 0 a b))

(fast-mul 5 7)  ; Expected: 35
(fast-mul 5 8)  ; Expected: 40
