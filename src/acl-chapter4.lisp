;; [[file:acl-chapter4.org::*Chapter 4.2: Binary Search][Chapter 4.2: Binary Search:1]]
(defun bin-search (obj vec)
  (let ((len (length vec)))
    (if (zerop len)
        nil
        (finder obj vec 0 (- len 1)))))             ; (9)

(defun finder (obj vec start end)                   ; (1)
  (let ((range (- end start)))                      ; (2)
    (if (zerop range)                               ; (3)
        (if (eql obj (aref vec start))              ; (4)
            start
            nil)
        (let ((mid (+ start (round (/ range 2)))))  ; (5)
          (let ((cur-obj (aref vec mid)))           ; (6)
            (if (< obj cur-obj)                     ; (7)
                (finder obj vec start (- mid 1))
                (if (> obj cur-obj)                 ; (8)
                    (finder obj vec (+ mid 1) end)
                    mid)))))))

;; NIL
(bin-search 41 #(1 3 4 6 7 8 10 13 14 18 19 21 24 27 40 45 71))

;; 4
(bin-search 7 #(1 3 4 6 7 8 10 13 14 18 19 21 24 27 40 45 71))
;; Chapter 4.2: Binary Search:1 ends here
