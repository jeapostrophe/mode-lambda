#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/match)
(module+ test
  (require rackunit))

(define (color? x)
  ;; ARGB
  (and (bytes? x) (= 4 (bytes-length x))))
(define (alpha x) (bytes-ref x 0))
(define (red x) (bytes-ref x 1))
(define (green x) (bytes-ref x 2))
(define (blue x) (bytes-ref x 3))

(define (argb a r g b)
  (bytes a r g b))

(module+ test
  (require racket/format)
  (define (show-color c)
    (~a (~a #:min-width 3 #:align 'right (alpha c)) " "
        (~a #:min-width 3 #:align 'right (red c)) " "
        (~a #:min-width 3 #:align 'right (green c)) " "
        (~a #:min-width 3 #:align 'right (blue c)) " "))
  (define (show-table f l)
    (define X (fl->fx (flceiling (fl/ (log (add1 (length l))) (log 10)))))
    (for ([c (in-list l)]
          [i (in-naturals 1)])
      (displayln
       (~a (~a #:min-width X #:align 'right i) ". "
           (f c)))))
  (define (show-colors cs)
    (show-table show-color cs)))

(define TRANSPARENT
  (argb 0 0 0 0))
(define WHITE
  (argb 255 255 255 255))
(define BLACK
  (argb 255 0 0 0))
(define RED
  (argb 255 255 0 0))
(define GREEN
  (argb 255 0 255 0))
(define BLUE
  (argb 255 0 0 255))

(define (color->shades base how-many)
  (for/list ([i (in-range 1 how-many)])
    (define factor (fl/ (fx->fl i) (fx->fl how-many)))
    (define (mult component)
      (fl->fx (flround (fl* (fx->fl (component base)) factor))))
    (argb (alpha base) (mult red) (mult green) (mult blue))))
(module+ test
  (displayln "Shades")
  (show-colors (color->shades RED 4)))

(define (comp-cap x)
  (fxmin 255 x))

(define (color+ x y)
  (define (comp+ red)
    (comp-cap (fx+ (red x) (red y))))
  (argb (comp+ alpha) (comp+ red) (comp+ green) (comp+ blue)))
(define (inverse c)
  (argb (alpha c) (fx- 255 (red c)) (fx- 255 (green c)) (fx- 255 (blue c))))

(define (color->tint base how-many)
  (define shades (color->shades (inverse base) how-many))
  (for/list ([s (in-list shades)])
    (color+ base s)))
(module+ test
  (displayln "Tints")
  (show-colors (color->tint RED 4)))

(define (color->palette base)
  (append (list TRANSPARENT)
          (color->shades base 8)
          (list base)
          (color->tint base 8)))
(module+ test
  (displayln "Palette")
  (show-colors (color->palette RED)))

(define (argb% a r g b)
  (define (scale x)
    (fl->fx (flround (fl* x 255.0))))
  (argb (scale a) (scale r) (scale g) (scale b)))
(module+ test
  (check-equal? (argb% 1.0 1.0 1.0 1.0)
                (argb 255 255 255 255))
  (check-equal? (argb% 0.0 0.0 0.0 0.0)
                (argb 0 0 0 0))
  (check-equal? (argb% 0.5 0.5 0.5 0.5)
                (argb 128 128 128 128)))

(define (ahsv a h s v)
  (define (ret r g b)
    (argb% a r g b))
  (cond
   [(fl<= s 0.0)
    (ret v v v)]
   [else
    (define hh
      (fl/ (if (fl>= h 360.0) 0.0 h) 60.0))
    (define i
      (fl->fx (flfloor hh)))
    (define ff
      (fl- hh (fx->fl i)))
    (define p
      (fl* v (fl- 1.0 s)))
    (define q
      (fl* v (fl- 1.0 (fl* s ff))))
    (define t
      (fl* v (fl- 1.0 (fl* s (fl- 1.0 ff)))))
    (match i
      [0 (ret v t p)]
      [1 (ret q v p)]
      [2 (ret p v t)]
      [3 (ret p q v)]
      [4 (ret t p v)]
      [_ (ret v p q)])]))
(module+ test
  (displayln "AHSV")
  (show-colors
   (for/list ([i (in-range 10)])
     (ahsv 1.0 (fl* 360.0 (random)) (random) (random))))
  (check-equal? (argb 255 0 255 255)
                (ahsv 1.0 180.0 1.0 1.0)))

(define (color-wheel how-many)
  (for/list ([i (in-range 0 how-many)])
    (ahsv 1.0 (fl* 360.0 (fl/ (fx->fl i) (fx->fl how-many))) 1.0 1.0)))
(module+ test
  (displayln "Color Wheel")
  (show-colors
   (color-wheel 13)))

;; xxx harmonies
