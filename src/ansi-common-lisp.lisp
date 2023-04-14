;; [[file:ansi-common-lisp.org::*Chapter 2.10: Variables][Chapter 2.10: Variables:1]]
(defun test-let ()
  (let ((var1 5)
        (var2 13)
        (var3 10))
    (+ var1 var2 var3))
  (let ((var1 7))       ; Note the list of declarations even with only 1
    (- var1 5)))

(test-let)
;; Chapter 2.10: Variables:1 ends here

;; [[file:ansi-common-lisp.org::*Chapter 2.13: Iteration][Chapter 2.13: Iteration:1]]
(defun test-do (n)
  (do ((i 0 (+ i 1))
       (j 0 (+ j 3)))
      ((> i n)              ; Stop condition
       (format t "Finished first loop.~%")      ; Will get executed once it stops
       'done)               ; Last expresion, returned by do macro
    (format t "i: ~A j: ~A~%" i j))
  (do ((i n (- i 2)))       ; Note the list of declarations even with only 1
      ((<= i 0) 'done)
    (format t "i: ~A~%" i)))

(test-do 5)
;; Chapter 2.13: Iteration:1 ends here

;; [[file:ansi-common-lisp.org::*Chapter 2.14: Functions as Objects][Chapter 2.14: Functions as Objects:1]]
(lambda (x y)       ; No name
  (* x y))
;; Chapter 2.14: Functions as Objects:1 ends here

;; [[file:ansi-common-lisp.org::*Chapter 2.14: Functions as Objects][Chapter 2.14: Functions as Objects:2]]
((lambda (x) (* x x)) 5)    ; 5 is a parameter
;; Chapter 2.14: Functions as Objects:2 ends here

;; [[file:ansi-common-lisp.org::*Chapter 2.14: Functions as Objects][Chapter 2.14: Functions as Objects:3]]
(let ((mylambda #'(lambda (x y)
                  (format t "I am an expression inside the lambda!~%")
                  (* x y))))
  (format t "Begining of the let expression block!~%")
  (format t "Lambda return: ~A~%" (funcall mylambda 5 3)))
;; Chapter 2.14: Functions as Objects:3 ends here

;; [[file:ansi-common-lisp.org::*Chapter 2.14: Functions as Objects][Chapter 2.14: Functions as Objects:4]]
;;  'symbol -> (quote symbol)
;; #'symbol -> (function symbol)
(funcall #'(lambda (x)
             (format t "(funcall) Value of x: ~A~%" x)) ; Body of lambda
         "Test 1")                                      ; Argument

(apply #'(lambda (x)
           (format t "  (apply) Value of x: ~A~%" x))   ; Body of lambda
       '("Test 2"))                                     ; Argument. Needs list
;; Chapter 2.14: Functions as Objects:4 ends here
