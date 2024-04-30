(defun summation (a b f)
  (if (> a b)
      0
    (+ (funcall f a)
       (summation (+ a 1) b f))))

(princ (summation 1 5 (lambda (x) x)))
(princ ", ")
(princ (summation 1 5 (lambda (x) (expt x 2))))
