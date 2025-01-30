;; Exercise 1.41
;; =============

(defun double (f)
  (lambda (arg)
    (f (f arg))))

(((double (double double)) inc) 5)  ; Expected: 5+16=21

;; Exercise 1.42
;; =============

(defun compose (f g)
  (lambda (arg)
    (f (g arg))))

;; Exercise 1.43
;; =============

(defun repeated (f times)
  (if (<= times 1)
      f
      (compose f (repeated f (- times 1)))))
