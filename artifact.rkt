#lang racket/base
(require
  racket/gui
  racket/fixnum
  mode-lambda
  mode-lambda/static
  mode-lambda/backend/gl)

(define W 800)
(define H 800)

(define sd (make-sprite-db))

(let ()
  (local-require pict)
  (add-sprite!/value sd 'blue (colorize (filled-rectangle 3 3) "blue")))

(define csd (compile-sprite-db sd))
(define NLAYERS 2)
(define (layers)
  (for/vector ((i NLAYERS))
    (layer (fx->fl W) (fx->fl H))))

(define render (stage-draw/dc csd W H NLAYERS))
(gl-smoothing? #t)

(define (draw-screen canvas dc)
  (define s (sprite (- 0.0 (/ W 2)) (- 0.0 (/ H 2)) (sprite-idx csd 'blue)))
  (define r (render (layers) '() (list s)))
  (r (send canvas get-width) (send canvas get-height) dc))

(define frame (new frame%
                   (label "mode-lambda pull request #11")
                   (width W)
                   (height H)))

(define glconfig (new gl-config%))
(send glconfig set-legacy? #f)

(define canvas
  (new canvas%
       (parent frame)
       (min-width W)
       (min-height H)
       (paint-callback draw-screen)
       (gl-config glconfig)
       (style '(no-autoclear gl))))

(send frame show #t)
