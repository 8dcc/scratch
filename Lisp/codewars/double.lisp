;; Kata: https://www.codewars.com/kata/53ee5429ba190077850011d4

(defpackage #:challenge/solution
  (:use #:cl)
  (:export #:double-integer))
(in-package #:challenge/solution)

(defun double-integer (n)
  (+ n n))
