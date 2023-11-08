(defun func-width-to-bits (n)
  (- (ash 1 n) 1))

(func-width-to-bits 6)

(defmacro macro-width-to-bits (n)
  `(- (ash 1 ,n) 1))

(macro-width-to-bits 6)

(defmacro height-to-rows (n x)
  `(make-list ,n ,x))

(height-to-rows 5 85)

(defun bitmap-rect (w h)
  (make-list h
             (- (ash 1 w) 1)))

(bitmap-rect 7 5)

(defun bitmap-rect (w h)
  (make-list h
             (- (ash 1 w) 1)))

;; For math-format-radix
(require 'calc-bin)

(mapcar (lambda (n)
          (let ((calc-number-radix 2))
            (math-format-radix n)))
        (bitmap-rect 7 5))
