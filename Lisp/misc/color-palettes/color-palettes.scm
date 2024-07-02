;; Generate a color palette of N elements, scaling the lightness by STEP, having
;; HSV color in the middle. The STEP must be in the [0..1] range.
(define (palette-monochrome hsv size step)
  (define (add-lightness hsv difference)
    (list (car hsv)
          (cadr hsv)
          (+ (caddr hsv) difference)))

  (cond ((<= size 1)
         (list hsv))
        ((= size 2)
         (list hsv (add-lightness hsv step)))
        (#t
         (let* ((rounded-size (if (odd? size) (- size 1) size))
                (lightness-difference (* step (/ rounded-size 2))))
           (append (list (add-lightness hsv (* -1 lightness-difference)))
                   (palette-monochrome hsv (- size 2) step)
                   (list (add-lightness hsv lightness-difference)))))))

(palette-monochrome '(240 1 0.70) 5 0.10)

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

(palette-generator '(240 1 0.70) 5
                   (lambda (hsv relative-idx)
                     (define lightness-step 0.10)
                     (list (car hsv)
                           (cadr hsv)
                           (+ (caddr hsv)
                              (* relative-idx lightness-step)))))

(define (add-degrees hsv degrees)
  (define (fmod x y)
    (if (< x y)
        x
        (fmod (- x y) y)))

  (list (fmod (+ (car hsv) degrees)
              360.0)
        (cadr hsv)
        (caddr hsv)))

(palette-generator '(240 1 0.9) 5
                   (lambda (hsv relative-idx)
                     (define degree-difference 30.0)
                     (add-degrees hsv (* relative-idx degree-difference))))

(map (lambda (hsv)
       (palette-monochrome hsv 3 0.10))
     '((180 1 0.9)
       (210 1 0.9)
       (240 1 0.9)
       (270 1 0.9)
       (300 1 0.9)))
