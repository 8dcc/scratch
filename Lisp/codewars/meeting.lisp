;; [[file:meeting.org::*Meeting][Meeting:1]]
(defun split-str (string c)                                 ; (1)
  (loop for i = 0 then (1+ j)
        as j = (position c string :start i)
        collect (subseq string i j)
        while j))

(defun upper-split (s)                                      ; (2)
  (mapcar #'(lambda (sub-str)
              (let ((sub-lst (split-str sub-str #\:)))
                (rotatef (car sub-lst) (cadr sub-lst))
                sub-lst))
          (split-str (string-upcase s) #\;)))

(defun meeting (s)
  (sort (upper-split s)
        #'(lambda (lst1 lst2)                               ; (3)
            (if (equal (car lst1) (car lst2))               ; (4)
                (string-lessp (cadr lst1) (cadr lst2))
                (string-lessp (car lst1) (car lst2))))))

;; Tests
(meeting "Alexis:Wahl;John:Bell;Victoria:Schwarz;Abba:Dorny;Grace:Meta;Ann:Arno;Madison:STAN;Alex:Cornwell;Lewis:Kern;Megan:Stan;Alex:Korn")
;; Meeting:1 ends here
