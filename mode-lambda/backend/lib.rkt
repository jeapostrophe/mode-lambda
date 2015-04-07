#lang racket/base
(require mode-lambda/core
         racket/draw
         racket/contract/base
         racket/class)

(define (argb-bytes->bitmap w h bs)
  (define root-bm (make-bitmap w h))
  (send root-bm set-argb-pixels 0 0 w h bs)
  root-bm)

(define (save-bitmap! bm p)
  (send bm save-file p 'png 100 #:unscaled? #t)
  (void))

(define (draw-bitmap! w W h H bm dc)
  (send dc set-background "white")
  (send dc clear)
  (define scale
    (min (/ w W) (/ h H)))
  (define SW (* scale W))
  (define SH (* scale H))
  (define x (/ (/ (- w SW) 2) scale))
  (define y (/ (/ (- h SH) 2) scale))
  (send dc set-scale scale scale)
  (send dc draw-bitmap bm x y)
  (void))

(define draw/dc/c
  (backend/c ()
             (-> exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 any/c
                 any)))

(provide
 (contract-out
  [argb-bytes->bitmap
   (-> exact-nonnegative-integer?
       exact-nonnegative-integer?
       bytes?
       (is-a?/c bitmap%))]
  [save-bitmap!
   (-> (is-a?/c bitmap%) 
       path-string?
       void?)]
  [draw-bitmap!
   (-> exact-nonnegative-integer? exact-nonnegative-integer?
       exact-nonnegative-integer? exact-nonnegative-integer?
       (is-a?/c bitmap%)
       (is-a?/c dc<%>)
       void?)]
  [draw/dc/c
   contract?]))
