#lang racket/base
(require racket/match
         racket/class
         mode-lambda
         mode-lambda/static
         mode-lambda/text/static
         mode-lambda/text/runtime
         mode-lambda/color
         mode-lambda/backend/gl
         racket/draw
         racket/gui/base
         pict
         pict/flash)

;;;
;;; SIZES
;;;

(define W 400)
(define H 400)
(define W/2 (/ W 2.))
(define H/2 (/ H 2.))

;;;
;;; BITMAPS
;;;

(define gray-bm
  (pict->bitmap (colorize (filled-rectangle W H) "gray")))
(define fish-bm
  (pict->bitmap (standard-fish  100 50)))
(define lantern-bm
  (pict->bitmap (jack-o-lantern 100)))
(define flash-bm
  (pict->bitmap (colorize (linewidth 5 (outline-flash 100 105)) "yellow")))

;;;
;;; SPRITES
;;;

(define db (make-sprite-db))

(add-sprite!/bm db 'gray    (λ() gray-bm))
(add-sprite!/bm db 'fish    (λ() fish-bm))
(add-sprite!/bm db 'lantern (λ() lantern-bm))
(add-sprite!/bm db 'flash   (λ() flash-bm))

(define cdb (compile-sprite-db db))

(module+ test
  (require racket/runtime-path)
  (define-runtime-path jens "jens")
  (save-csd! cdb jens #:debug? #t))

(define gray-idx    (sprite-idx cdb 'gray))
(define fish-idx    (sprite-idx cdb 'fish))
(define lantern-idx (sprite-idx cdb 'lantern))
(define flash-idx   (sprite-idx cdb 'flash))

;;;
;;; LAYERS
;;;

(define bugl (layer W/2 H/2))    ; gray:       layer 0 ; too see bugs in GL
(define bgl  (layer W/2 H/2))    ; background: layer 1
(define ml   (layer W/2 H/2))    ; middle:     layer 2
(define fgl  (layer W/2 H/2))    ; foreground: layer 3
(define lc   (vector bugl bgl ml fgl)) ; layer config

;;;
;;; SPRITES
;;;

(define gray-sprite    (sprite W/2  H/2  gray-idx    #:layer 0)) ; bug layer
(define fish-sprite    (sprite 200. 200. fish-idx    #:layer 1)) ; background
(define lantern-sprite (sprite 250. 200. lantern-idx #:layer 2)) ; middle
(define flash-sprite   (sprite 250. 200. flash-idx   #:layer 3)) ; foreground

;;;
;;; RUNTIME
;;;

(define rendering-states->draw (stage-draw/dc cdb W H (vector-length lc)))

(define static (list gray-sprite))

(define (paint-canvas c dc)
  (define dynamic (list flash-sprite))
  (define draw (rendering-states->draw lc static dynamic))
  (match/values (send c get-scaled-client-size)
    [(w h) (draw w h dc)]))

(module+ main
  (define glc (new gl-config%))
  (send glc set-legacy? #f)
  (define f (new frame% [label "Hello"] [width W] [height H]))
  (define c
    (new canvas%
         [parent f]
         [min-width W]
         [min-height H]
         [gl-config glc]
         [style '(no-autoclear gl)]
         [paint-callback paint-canvas]))

  (send f show #t))
