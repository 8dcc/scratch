
;; Exercise 2.18
;; =============
;;
;; Define a procedure reverse that takes a list as argument and returns a list
;; of the same elements in reverse order:
;;
;;   (reverse (list 1 4 9 16 25))
;;     => (25 16 9 4 1)
;;
;; Notes
;; -----
;;
;; FIXME: The evaluation of:
;;
;;   (reverse2 '(a b c))
;;
;; Returns (nil nil nil) in commit 7c4f3ba of SL. See:
;;
;;   https://github.com/8dcc/sl/issues/9

;; -----------------------------------------------------------------------------
;; Exercise

(defun reverse1 (lst)
  (defun init (lst)
    "Get all except the last element. Equivalent to Haskell's `init'."
    (cond ((or (null? lst) (null? (cdr lst))) nil)   ; nil or (a) => nil
          ((null? (cddr lst)) (list (car lst)))      ; (a b) => (a)
          (tru (cons (car lst) (init (cdr lst))))))  ; (a b c) => (a b)
  (defun last (lst)
    "Get the last element. Equivalent to Haskell's `init'."
    (cond ((null? lst) nil)
          ((null? (cdr lst)) (car lst))
          (tru (last (cdr lst)))))
  (if (null? lst)
      nil
      (cons (last lst)
            (reverse1 (init lst)))))

(defun reverse2 (lst)
  (if (null? lst)
      nil
      (append (reverse2 (cdr lst))
              (list (car lst)))))

(defun reverse3 (lst)
  (defun iter (remaining reversed)
    (if (null? remaining)
        reversed
        (iter (cdr remaining)
              (append reversed
                      (list (car remaining))))))
  (iter lst nil))

;; -----------------------------------------------------------------------------
;; Examples

(defun test-reverse (reverse-func)
  (list
   (reverse-func '())           ; Expected: nil
   (reverse-func '(a))          ; Expected: (a)
   (reverse-func '(a b c))      ; Expected: (c b a)
   (reverse-func '(a b c d))))  ; Expected: (d c b a)

(test-reverse reverse1)
(test-reverse reverse2)
(test-reverse reverse3)
