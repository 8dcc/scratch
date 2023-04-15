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

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:1]]
(+ (- 5 1) (+ 3 7))
;; Exercises:1 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:2]]
(list 1 (+ 2 3))
;; Exercises:2 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:3]]
(if (listp 1)
    (+ 1 2)
    (+ 3 4))
;; Exercises:3 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:4]]
(list (and (listp 3) t) (+ 1 2))
;; Exercises:4 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:5]]
(cons 'a (cons 'b (cons 'c nil)))
;; Exercises:5 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:6]]
(cons 'a '(b c))
;; Exercises:6 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:7]]
(cons 'a (cons 'b '(c)))
;; Exercises:7 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:8]]
(defun fourth-elem (lst)
  (car (cdr (cdr (cdr lst)))))

(fourth-elem '(10 20 30 40 50))
;; Exercises:8 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:9]]
(defun greater (x y)
  (if (> x y)
      x
      y))

(greater 5 12)
;; Exercises:9 ends here

;; [[file:ansi-common-lisp.org::*Exercises][Exercises:10]]
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(enigma '(1 2 3))
(enigma '(1 2 nil 4))
;; Exercises:10 ends here
