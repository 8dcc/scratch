
;; Exercise 1.30
;; =============
;;
;; The sum procedure above generates a linear recursion. The procedure can be
;; rewritten so that the sum is performed iteratively. Show how to do this by
;; filling in the missing expressions in the following definition:
;;
;;   (define (sum term a next b)
;;       (define (iter a result)
;;           (if ⟨??⟩
;;               ⟨??⟩
;;               (iter ⟨??⟩ ⟨??⟩)))
;;     (iter ⟨??⟩ ⟨??⟩))

(defun sum-recur (term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recur term (next a) next b))))

(defun sum-iter (term a next b)
  (defun iter (a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))

;; sum_(i=5)^10 2i = 90
(sum-recur (lambda (x) (* x 2))
           5
           (lambda (x) (+ x 1))
           10)

(sum-iter (lambda (x) (* x 2))
          5
          (lambda (x) (+ x 1))
          10)
