
;; Exercise 1.17
;; =============
;;
;; The exponentiation algorithms in this section are based on performing
;; exponentiation by means of repeated multiplication. In a similar way, one can
;; perform integer multiplication by means of repeated addition. The following
;; multiplication procedure (in which it is assumed that our language can only
;; add, not multiply) is analogous to the `expt' procedure:
;;
;;   (define (* a b)
;;     (if (= b 0)
;;         0
;;         (+ a (* a (- b 1)))))
;;
;; This algorithm takes a number of steps that is linear in `b'.  Now suppose we
;; include, together with addition, operations `double', which doubles an
;; integer, and `halve', which divides an (even) integer by 2. Using these,
;; design a multiplication procedure analogous to `fast-expt' that uses a
;; logarithmic number of steps.
;;
;;------------------------------------------------------------------------------
;; First, the auxiliary functions.

(defun even? (n)
  (equal? (mod n 2) 0))
(defun double (n)
  (* n 2))
(defun halve (n)
  (/ n 2))

;;------------------------------------------------------------------------------
;; The recursive version, equivalent to the original `fast-expt' from SICP.

(defun fast-mul (a b)
  (cond ((equal? b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

(fast-mul 5 7)  ; Expected: 35
(fast-mul 5 8)  ; Expected: 40

;;------------------------------------------------------------------------------
;; And the iterative version, equivalent to Exercise 1.16.

(defun fast-mul (a b)
  (defun iter (total a b)
    (cond ((equal? b 0) total)
          ((even? b) (iter total (double a) (halve b))) ; a *= 2; b /= 2;
          (else (iter (+ total a) a (- b 1)))))         ; total += a; b -= 1;
  (iter 0 a b))

(fast-mul 5 7)  ; Expected: 35
(fast-mul 5 8)  ; Expected: 40
