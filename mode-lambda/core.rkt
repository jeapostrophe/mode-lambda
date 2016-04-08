#lang racket/base
(require racket/match
         racket/contract/base
         racket/list
         (except-in ffi/unsafe ->)
         mode-lambda/util)

(define PALETTE-DEPTH 16)

(struct compiled-sprite-db
  (atlas-size atlas-bs spr->idx idx->w*h*tx*ty pal-size pal-bs pal->idx))

(define LAYERS 8)
(define layer/c
  (and/c byte? (between/c 0 (sub1 LAYERS))))

(define-cstruct _layer-data
  ([cx _float]
   [cy _float]
   [hw _float]
   [hh _float]
   [mx _float]
   [my _float]
   [theta _float]
   [mode7-coeff _float]
   [horizon _float]
   [fov _float]
   [wrap-x? _bool]
   [wrap-y? _bool]))
(module+ test
  (printf "layer-data size: ~v bytes\n" (ctype-sizeof _layer-data)))

(define default-layer
  (make-layer-data 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 1.0 #f #f))
(define default-layer-config
  (make-vector LAYERS #f))

;; Mode7 Docs:
;; - http://www.coranac.com/tonc/text/mode7.htm
;; - http://unspecified.wordpress.com/2012/06/21/calculating-the-gluperspective-matrix-and-other-opengl-matrix-maths/
;; - http://stackoverflow.com/questions/22886853/how-to-convert-projected-points-to-screen-coordinatesviewport-matrix
;; - http://scratchapixel.com/lessons/3d-advanced-lessons/perspective-and-orthographic-projection-matrix/perspective-projection-matrix/
;; - https://en.wikipedia.org/wiki/Mode_7
;; - http://gamedev.stackexchange.com/questions/24957/doing-an-snes-mode-7-affine-transform-effect-in-pygame
;; - http://stackoverflow.com/questions/15844101/projection-transforming-3d-to-2d
;; - http://stackoverflow.com/questions/724219/how-to-convert-a-3d-point-into-2d-perspective-projection

;; FOV should be a multiple of the aspect ratio to look good,
;; when it is small, it is more zoomed.

;; (coeff = 0.0) & FOV: 1.0 -- no mode7

;; (coeff = 1.0) If Z increases towards top of screen, then it
;; is a ceiling and Hor-Y is Positive and when negative, Z
;; should be +inf.0

;; (coeff = 2.0) If Z increases towards bot of screen, then it
;; is a floor and Hor-Y is Negative and when positive, Z
;; should be +inf.0

;; (coeff = 3.0) If Z increases away from horizon, then it is
;; a cylinder and we want the absolute value of the distance

(define-cstruct&list 
  _sprite-data _sprite-data:info
  ([dx _float]       ;; 0            0
   [dy _float]       ;; 1            4
   [mx _float]       ;; 2            8
   [my _float]       ;; 3           12
   [theta _float]    ;; 4           16
   [a _float]        ;; 5           20
   [spr _ushort]     ;; 6           24
   [pal _ushort]     ;; 7           26
   [layer _byte]     ;; 8           28
   [r _byte]         ;; 9           29
   [g _byte]         ;; 10          30
   [b _byte]         ;; 11          31
   ))

(module+ test
  (printf "sprite-data size: ~v bytes\n" (ctype-sizeof _sprite-data)))

(define layer-vector/c
  (apply vector/c (make-list LAYERS (or/c false/c layer-data?))))

(define-syntax-rule (backend/c (addl-input ...) output)
  (-> layer-vector/c tree/c tree/c addl-input ... output))

(define draw!/c
  (backend/c () void?))
(define render/c
  (backend/c () bytes?))

(define (stage-backend/c output/c)
  (-> compiled-sprite-db?
      exact-nonnegative-integer?
      exact-nonnegative-integer?
      output/c))

(provide
 (all-defined-out))
