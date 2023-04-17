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

;; [[file:ansi-common-lisp.org::*Expression 1][Expression 1:1]]
(+ (- 5 1) (+ 3 7))
;; Expression 1:1 ends here

;; [[file:ansi-common-lisp.org::*Expression 2][Expression 2:1]]
(list 1 (+ 2 3))
;; Expression 2:1 ends here

;; [[file:ansi-common-lisp.org::*Expression 3][Expression 3:1]]
(if (listp 1)
    (+ 1 2)
    (+ 3 4))
;; Expression 3:1 ends here

;; [[file:ansi-common-lisp.org::*Expression 4][Expression 4:1]]
(list (and (listp 3) t) (+ 1 2))
;; Expression 4:1 ends here

;; [[file:ansi-common-lisp.org::*Exercise 2][Exercise 2:1]]
(cons 'a (cons 'b (cons 'c nil)))
;; Exercise 2:1 ends here

;; [[file:ansi-common-lisp.org::*Exercise 2][Exercise 2:2]]
(cons 'a '(b c))
;; Exercise 2:2 ends here

;; [[file:ansi-common-lisp.org::*Exercise 2][Exercise 2:3]]
(cons 'a (cons 'b '(c)))
;; Exercise 2:3 ends here

;; [[file:ansi-common-lisp.org::*Exercise 3][Exercise 3:1]]
(defun fourth-elem (lst)
  (car (cdr (cdr (cdr lst)))))

(fourth-elem '(10 20 30 40 50))
;; Exercise 3:1 ends here

;; [[file:ansi-common-lisp.org::*Exercise 4][Exercise 4:1]]
(defun greater (x y)
  (if (> x y)
      x
      y))

(greater 5 12)
;; Exercise 4:1 ends here

;; [[file:ansi-common-lisp.org::*Function 1][Function 1:1]]
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(enigma '(1 2 3))
(enigma '(1 2 nil 4))
;; Function 1:1 ends here

;; [[file:ansi-common-lisp.org::*Function 2][Function 2:1]]
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

(mystery 4 '(1 2 3))
(mystery 5 '(1 3 5 7))
;; Function 2:1 ends here

;; [[file:ansi-common-lisp.org::*Expression 1][Expression 1:1]]
(car (car (cdr '(a (b c) d))))
;; Expression 1:1 ends here

;; [[file:ansi-common-lisp.org::*Expression 2][Expression 2:1]]
(or 13 (/ 1 0))
;; Expression 2:1 ends here

;; [[file:ansi-common-lisp.org::*Expression 3][Expression 3:1]]
(apply #'list 1 nil)
;; Expression 3:1 ends here

;; [[file:ansi-common-lisp.org::*Exercise 7][Exercise 7:1]]
(defun has-list (x)
  (if (null x)
      nil
      (if (listp (car x))
          t
          (has-list (cdr x)))))

(has-list '(1 2 3))             ; nil
(has-list '(1 nil 3))           ; t (nil is an empty list)
(has-list '(1 (25 26 27) 3))    ; t
;; Exercise 7:1 ends here

;; [[file:ansi-common-lisp.org::*Function 1][Function 1:1]]
(defun dots-iter (x)
  (do ((i 0 (+ i 1)))
      ((>= i x) 'done)
    (format t ".")))

(dots-iter 5)
;; Function 1:1 ends here

;; [[file:ansi-common-lisp.org::*Function 1][Function 1:2]]
(defun dots-recur (x)
  (format t ".")
  (if (<= x 1)
      'done
      (dots-recur (- x 1))))

(dots-recur 5)
;; Function 1:2 ends here

;; [[file:ansi-common-lisp.org::*Function 2][Function 2:1]]
(defun app-iter (x y)
  (let ((ret 0))
    (dolist (i y)
      (if (= x i)
          (setf ret (+ ret 1))))
    ret))

(app-iter 5 '(1 2 3 4))     ; 0
(app-iter 1 '(1 8 1 9 1))   ; 3
;; Function 2:1 ends here

;; [[file:ansi-common-lisp.org::*Function 2][Function 2:2]]
(defun app-recur (x y)
  (if (null y)
      0
      (if (= x (car y))
          (+ 1 (app-recur x (cdr y)))   ; Match
          (app-recur x (cdr y)))))      ; No mach

(app-recur 5 '(1 2 3 4))    ; 0
(app-recur 1 '(1 8 1 9 1))  ; 3
;; Function 2:2 ends here

;; [[file:ansi-common-lisp.org::*Function 1][Function 1:1]]
(defun summit (lst)
  (remove nil lst)
  (apply #'+ lst))
;; Function 1:1 ends here

;; [[file:ansi-common-lisp.org::*Function 1][Function 1:2]]
(defun summit (lst)
  (apply #'+ (remove nil lst)))

(summit '(1 nil 2 nil 3))
;; Function 1:2 ends here

;; [[file:ansi-common-lisp.org::*Function 2][Function 2:1]]
(defun summit (lst)
  (let ((x (car lst)))
    (if (null x)
        (summit (cdr lst))
        (+ x (summit (cdr lst))))))
;; Function 2:1 ends here

;; [[file:ansi-common-lisp.org::*Function 2][Function 2:2]]
(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))))

(summit '(1 nil 2 nil 3))
;; Function 2:2 ends here

;; [[file:ansi-common-lisp.org::*Chapter 3.3: Why lisp has no pointers][Chapter 3.3: Why lisp has no pointers:1]]
(setf x '(1 2 3 4))
(setf y x)
(format t "x:  ~A, y: ~A, eql: ~A~%" x y (eql x y))

(setf x '(10 20 30))    ; y will point to the list from the first setf
(format t "x: ~A, y: ~A, eql: ~A~%" x y (eql x y))
;; Chapter 3.3: Why lisp has no pointers:1 ends here

;; [[file:ansi-common-lisp.org::*Chapter 3.3: Why lisp has no pointers][Chapter 3.3: Why lisp has no pointers:3]]
(setf x '(1 2 3 4))
(setf y x)
(format t "x: ~A, y: ~A, eql: ~A~%" x y (eql x y))

(setf (car x) 0         ; y gets updated as well
      (third x) 0)
(format t "x: ~A, y: ~A, eql: ~A~%" x y (eql x y))
;; Chapter 3.3: Why lisp has no pointers:3 ends here
