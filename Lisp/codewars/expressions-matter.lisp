(defun expressions-matter (a b c)
  "Return the highest achievable result."
  (max (+ a b c)
       (* a b c)
       (* a (+ b c))
       (+ a (* b c))
       (* (+ a b) c)))

(expressions-matter 2 1 2)
