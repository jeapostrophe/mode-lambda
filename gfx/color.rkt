#lang racket/base
(require racket/contract/base
         racket/fixnum
         racket/flonum
         racket/match
         racket/list
         (only-in srfi/1 iota))
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
  (append (list TRANSPARENT BLACK)
          (color->shades base 7)
          (list base)
          (color->tint base 7)
          (list WHITE)))
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

(define GRAY
  (ahsv 1.0 0.0 0.0 0.5))

(define (color-wheel how-many #:s [s 1.0] #:b [b 1.0])
  (for/list ([i (in-range 0 how-many)])
    (ahsv 1.0 (fl* 360.0 (fl/ (fx->fl i) (fx->fl how-many))) s b)))
(module+ test
  (displayln "Color Wheel")
  (show-colors
   (color-wheel 12)))

;; http://www.tigercolor.com/color-lab/color-theory/color-theory-intro.htm
(define (complement-idxs hm)
  (define half (quotient hm 2))
  (for/list ([x (iota half)]
             [y (iota half half)])
    (vector x y)))
(module+ test
  ;; Should be divisible by 3 and 4
  (define harmony-wheel (* 2 3 4))
  (define simple-wheel (color-wheel harmony-wheel))
  (define (show-color-vector x)
    (apply ~a
           (add-between
            (for/list ([e (in-vector x)])
              (show-color (list-ref simple-wheel e)))
            "-> ")))
  (displayln "Complements")
  (define complements (complement-idxs harmony-wheel))
  complements
  (show-table show-color-vector complements))

(define (analogous-idxs hm)
  (for/list ([x (iota hm)])
    (vector (modulo (+ x 1) hm) x (modulo (- x 1) hm))))
(module+ test
  (displayln "Analogous")
  (define analogous (analogous-idxs harmony-wheel))
  analogous
  (show-table show-color-vector analogous))

(define (triadic-idxs hm)
  (define third (quotient hm 3))
  (for/list ([x (iota third)]
             [y (iota third third)]
             [z (iota third (* 2 third))])
    (vector x y z)))
(module+ test
  (displayln "Triadic")
  (define triadic (triadic-idxs harmony-wheel))
  triadic
  (show-table show-color-vector triadic))

(define (split-complementary-idxs hm)
  (define half (quotient hm 2))
  (for/list ([x (iota hm)]
             [y (iota hm half)])
    (vector x (modulo (+ y 1) hm) (modulo (- y 1) hm))))
(module+ test
  (displayln "Split-Complementary")
  (define split-complementary (split-complementary-idxs harmony-wheel))
  split-complementary
  (show-table show-color-vector split-complementary))

(define (tetradic-idxs hm)
  (define half (quotient hm 2))
  (for/list ([x (iota hm)]
             [y (iota hm half)])
    (vector (modulo (+ x 1) hm) (modulo (- x 1) hm)
            (modulo (+ y 1) hm) (modulo (- y 1) hm))))
(module+ test
  (displayln "Tetradic")
  (define tetradic (tetradic-idxs harmony-wheel))
  tetradic
  (show-table show-color-vector tetradic))

(define (square-idxs hm)
  (define fourth (quotient hm 4))
  (for/list ([a (iota fourth (* 0 fourth))]
             [b (iota fourth (* 1 fourth))]
             [c (iota fourth (* 2 fourth))]
             [d (iota fourth (* 3 fourth))])
    (vector a b c d)))
(module+ test
  (displayln "Square")
  (define square (square-idxs harmony-wheel))
  square
  (show-table show-color-vector square))

(provide
 (contract-out
  [GRAY color?]
  [color?
   (-> any/c
       boolean?)]
  [color->palette
   (-> color?
       (listof color?))]
  [color-wheel
   (->* (exact-nonnegative-integer?)
        (#:s (real-in 0.0 1.0)
         #:b (real-in 0.0 1.0))
        (listof color?))]
  [tetradic-idxs
   (-> exact-nonnegative-integer?
       (listof (vector/c exact-nonnegative-integer? exact-nonnegative-integer? 
                         exact-nonnegative-integer? exact-nonnegative-integer?)))]
  [square-idxs
   (-> exact-nonnegative-integer?
       (listof (vector/c exact-nonnegative-integer? exact-nonnegative-integer? 
                         exact-nonnegative-integer? exact-nonnegative-integer?)))]))
