(define (rgb2hsv rgb)
  (let* ((r (/ (car rgb) 255))
         (g (/ (cadr rgb) 255))
         (b (/ (caddr rgb) 255))
         (cmax (max r g b))
         (cmin (min r g b))
         (delta (- cmax cmin)))
    (list
     (cond ((= cmax r) (* 60 (modulo (/ (- g b) delta) 6)))
           ((= cmax g) (* 60 (+ (/ (- b r) delta) 2)))
           ((= cmax b) (* 60 (+ (/ (- r g) delta) 4)))
           (#t 0))
     (cond ((= cmax 0) 0)
           (#t (/ delta cmax)))
     cmax)))

;; For showing output as decimal
(map (lambda (x) (* x 1.0))
     (rgb2hsv '(17 34 238)))

(define (hsv2rgb hsv)
  ;; Is VALUE in the [A,B) range?
  (define (between a value b)
    (and (<= a value) (< value b)))

  ;; Calculate X mod Y, supporting floating point. X and Y must be positive.
  (define (fmod x y)
    (if (< x y)
        x
        (fmod (- x y) y)))

  (let* ((h (car hsv))
         (s (cadr hsv))
         (v (caddr hsv))
         (c (* s v))
         (hprime (/ h 60))
         (x (* c (- 1 (abs (- (fmod hprime 2) 1)))))
         (m (- v c))
         (rgb (cond ((between   0 h  60) (list c x 0))
                    ((between  60 h 120) (list x c 0))
                    ((between 120 h 180) (list 0 c x))
                    ((between 180 h 240) (list 0 x c))
                    ((between 240 h 300) (list x 0 c))
                    ((between 300 h 360) (list c 0 x))
                    (#t                  (list 0 0 0)))))
    (map (lambda (component)
           (round (* 255 (+ component m))))
         rgb)))

(hsv2rgb '(235.38 0.92 0.93))

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
