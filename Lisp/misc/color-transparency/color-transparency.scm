(define (rgb-add a b)
  (if (null? b)
      a
      (list (+ (car   a) (car   b))
            (+ (cadr  a) (cadr  b))
            (+ (caddr a) (caddr b)))))

(define (rgb-scale rgb scale)
  (list (* scale (car   rgb))
        (* scale (cadr  rgb))
        (* scale (caddr rgb))))

(define (transparencies-explicit colors transparencies)
  (if (or (null? colors)
          (null? transparencies))
      '()
      (rgb-add (rgb-scale (car colors) (car transparencies))
               (transparencies-explicit (cdr colors) (cdr transparencies)))))

;; (196 170 99)
(transparencies-explicit '((255 110 66) (232 218 178) (75 238 104))
                         '(0.5 0.2 0.3))

(define (transparencies-implicit colors transparencies)
  (define (apply-transparency c1 c2 t2)
    (rgb-add (rgb-scale c1 (- 1 t2))
             (rgb-scale c2 t2)))

  (define (transparencies-implicit-reversed colors transparencies)
    (if (or (null? (cdr colors)))
        (car colors)
        (let ((cur-transparency (car transparencies)))
          (rgb-add (rgb-scale (car colors)
                              cur-transparency)
                   (rgb-scale (transparencies-implicit-reversed (cdr colors) (cdr transparencies))
                              (- 1 cur-transparency))))))

  (transparencies-implicit-reversed (reverse colors)
                                    (reverse transparencies)))

;; (161 190 101)
(transparencies-implicit '((255 110 66) (232 218 178) (75 238 104))
                         '(1 0.3 0.5))
