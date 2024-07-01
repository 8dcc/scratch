;; Generate a color palette of N elements, scaling the lightness by STEP, having
;; HSV color in the middle. The STEP must be in the [0..1] range.
(define (palette-monochrome hsv size step)
  (define (scale-lightness hsv scale)
    (list (car hsv)
          (cadr hsv)
          (* (caddr hsv) (+ 1 scale))))

  (cond ((<= size 1)
         (list hsv))
        ((= size 2)
         (list hsv (scale-lightness hsv step)))
        (#t
         (let ((rounded-size (if (odd? size) (- size 1) size)))
           (append (list (scale-lightness hsv (* step (/ rounded-size 2) -1)))
                   (palette-monochrome hsv (- size 2) step)
                   (list (scale-lightness hsv (* step (/ rounded-size 2)))))))))

(palette-monochrome '(240 100 70) 5 0.10)

;; Generate a color palette of N elements, having HSV color in the middle. For
;; each color in the palette, the base color along with an index relative to the
;; center of the palette will be passed to the provided FUNC.
(define (palette-generator hsv size func)
  (cond ((<= size 1)
         (list (func hsv 0)))
        ((= size 2)
         (list (func hsv 0)
               (func hsv 1)))
        (#t
         (let* ((rounded-size (if (odd? size) (- size 1) size)))
           (append (list (func hsv (* -1 (/ rounded-size 2))))
                   (palette-generator hsv (- size 2) func)
                   (list (func hsv (/ rounded-size 2))))))))

(palette-generator '(240 100 70) 5
                   (lambda (hsv relative-idx)
                     (define scale-step 0.10)
                     (list (car hsv)
                           (cadr hsv)
                           (* (caddr hsv)
                              (+ 1 (* relative-idx scale-step))))))

(define (add-degrees hsv degrees)
  (define (fmod x y)
    (if (< x y)
        x
        (fmod (- x y) y)))

  (list (fmod (+ (car hsv) degrees)
              360.0)
        (cadr hsv)
        (caddr hsv)))

(palette-generator '(240 100 100) 5
                   (lambda (hsv relative-idx)
                     (define degree-difference 30.0)
                     (add-degrees hsv (* relative-idx degree-difference))))

(map (lambda (hsv)
       (palette-monochrome hsv 3 0.10))
     '((180 100 100)
       (210 100 100)
       (240 100 100)
       (270 100 100)
       (300 100 100)))
