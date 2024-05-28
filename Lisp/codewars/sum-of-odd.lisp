;; [[file:sum-of-odd.org::*Sum of odd][Sum of odd:1]]
(defun row-sum-odd-numbers (n)
  (let ((center-of-row (* n n)))        ; (1)
    (* center-of-row n)))

(row-sum-odd-numbers 3)
;; Sum of odd:1 ends here

;; [[file:sum-of-odd.org::*Sum of odd][Sum of odd:2]]
(defun row-sum-odd-numbers (n)
  (* n n n))

;; OR

(defun row-sum-odd-numbers-alt (n)
  (expt n 3))

(list (row-sum-odd-numbers 3)
      (row-sum-odd-numbers-alt 3))
;; Sum of odd:2 ends here
