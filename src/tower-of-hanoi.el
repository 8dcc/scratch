(defun my-print-list (lst)
  (if (not (null lst))
      (progn
        (princ (car lst))
        (my-print-list (cdr lst)))))

(defun move-stack (num from to aux)
  (if (> num 0)
      (progn
        (move-stack (- num 1) from aux to)
        (my-print-list (list "Moving from " from " to " to "\n"))
        (move-stack (- num 1) aux to from))))

(move-stack 4 'A 'B 'C)
