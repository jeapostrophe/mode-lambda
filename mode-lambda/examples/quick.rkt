#lang racket/base
(require racket/match
         racket/class
         racket/draw
         racket/gui/base
         mode-lambda         
         mode-lambda/backend/gl)
(provide quick-run)

(define (quick-run cdb static dynamic
                   #:width [WIDTH 800]
                   #:height [HEIGHT 600])
  (define ml-layers
    (make-vector 8 (layer (* WIDTH 0.5) (* HEIGHT 0.5))))
  (define ml-draw
    (stage-draw/dc cdb WIDTH HEIGHT (vector-length ml-layers)))

  (define glc (new gl-config%))
  (send glc set-hires-mode #t)
  (send glc set-legacy? #f)
  (define f
    (new frame% [label "Quick"] [width WIDTH] [height HEIGHT]))
  (define c
    (new canvas%
         [parent f]
         [min-width WIDTH]
         [min-height HEIGHT]
         [gl-config glc]
         [style '(no-autoclear gl)]
         [paint-callback
          (λ (c dc)
            (define draw (ml-draw ml-layers static dynamic))
            (match/values (send c get-gl-client-size)
              [(w h) (draw w h dc)]))]))

  (send f show #t))
