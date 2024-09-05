
;; Exercise 1.12
;; =============
;;
;; The following pattern of numbers is called Pascal's triangle.
;;
;;       1
;;      1 1
;;     1 2 1
;;    1 3 3 1
;;   1 4 6 4 1
;;
;; The numbers at the edge of the triangle are all 1, and each number inside the
;; triangle is the sum of the two numbers above it. Write a procedure that
;; computes elements of Pascal’s triangle by means of a recursive process.
;;
;;------------------------------------------------------------------------------
;; Sum each number of a list to its adjacent one, and return the sums in a
;; list. The returned list is 1 item shorter than the input.
(defun sum-pairs (list)
  (if (equal? (cdr list) nil)
      nil
      (cons (+ (car list) (cadr list))
            (sum-pairs (cdr list)))))

(sum-pairs '(1 3 3 1))    ; Expected: (4 6 4)
(sum-pairs '(1 4 6 4 1))  ; Expected: (5 10 10 5)

;;------------------------------------------------------------------------------
;; Return a list representing the N-th row of Pascal's triangle.
;;
;; We don't need to check if n is 2 because (sum-pairs '(1)) returns `nil', and
;; we can append 1 to each end of the list.
(defun get-pascal-row (n)
  (if (equal? n 1)
      '(1)
      (append '(1)
              (sum-pairs (get-pascal-row (- n 1)))
              '(1))))

(get-pascal-row 1)  ; Expected: (1)
(get-pascal-row 2)  ; Expected: (1 1)
(get-pascal-row 5)  ; Expected: (1 4 6 4 1)
