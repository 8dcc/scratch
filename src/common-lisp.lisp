(defun let-test ()
  (let ((var1 5)
        (var2 13)
        (var3 10))
    (+ var1 var2 var3))
  (let ((var1 7))           ; Note the list of declarations even with only 1
    (- var1 5)))

(let-test)

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
