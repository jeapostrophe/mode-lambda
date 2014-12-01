#lang racket/base
(require racket/class)

(define (argb-bytes->bitmap w h bs)
  (local-require racket/draw)
  (define root-bm (make-bitmap w h))
  (send root-bm set-argb-pixels 0 0 w h bs)
  root-bm)

(define (save-bitmap! bm p)
  (send bm save-file p 'png 100 #:unscaled? #t))

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
  (send dc draw-bitmap bm x y))

(provide
 ;; xxx
 (all-defined-out))
