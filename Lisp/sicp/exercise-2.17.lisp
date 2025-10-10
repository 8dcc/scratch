
;; Exercise 2.17
;; =============
;;
;; Define a procedure `last-pair' that returns the list that contains only the
;; last element of a given (nonempty) list:
;;
;;   (last-pair (list 23 72 149 34))
;;     => (34)

;; -----------------------------------------------------------------------------
;; Exercise

(defun last-pair (lst)
  (cond ((null? lst) nil)
        ((null? (cdr lst)) lst)
        (tru (last-pair (cdr lst)))))

;; -----------------------------------------------------------------------------
;; Examples

(last-pair '())                 ; Expected: nil
(last-pair '(12))               ; Expected: (12)
(last-pair '(23 72 149 34))     ; Expected: (34)
