(defun summation (a b f)
  (if (> a b)
      0
    (+ (funcall f a)
       (summation (+ a 1) b f))))

(list (summation 1 5 (lambda (x) x))
      (summation 1 5 (lambda (x) (expt 2 x))))

(defun summation (a b f)
  (defun iter (i total)
    (if (> i b)
        total
      (iter (+ i 1)
            (+ total (funcall f i)))))
  (iter a 0))

(list (summation 1 5 (lambda (x) x))
      (summation 1 5 (lambda (x) (expt 2 x))))
