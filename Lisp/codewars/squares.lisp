(defun is-square? (n)
  (and (>= n 0)
       (= (floor (sqrt n))
          (ceiling (sqrt n)))))

;; Another way of checking for decimals
(defun is-square? (n)
  (and (>= n 0)
       (zerop (mod (sqrt n) 1))))

(defun test-types (n)
  (type-of (sqrt n)))

(format t "~3A | ~A~%" (is-square? -1) (test-types -1))
(format t "~3A | ~A~%" (is-square? 0) (test-types 0))
(format t "~3A | ~A~%" (is-square? 3) (test-types 3))
(format t "~3A | ~A~%" (is-square? 4) (test-types 4))
