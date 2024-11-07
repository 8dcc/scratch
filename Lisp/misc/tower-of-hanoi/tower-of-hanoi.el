(defun print-items (lst)
  (dolist (e lst)
    (princ e)))

(defun move-stack (num from to aux)
  (when (> num 0)
    (move-stack (- num 1) from aux to)
    (print-items (list "Moving from " from " to " to "\n"))
    (move-stack (- num 1) aux to from)))

(move-stack 4 'A 'B 'C)
