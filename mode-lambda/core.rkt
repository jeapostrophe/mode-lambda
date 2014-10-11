#lang racket/base
(require racket/match
         racket/contract/base
         racket/list
         (except-in ffi/unsafe ->))

(define PALETTE-DEPTH 16)

(struct compiled-sprite-db 
  (atlas-size atlas-bs spr->idx idx->w*h*tx*ty pal-size pal-bs pal->idx))

(define LAYERS 8)
(define layer/c
  (and/c byte? (between/c 0 (sub1 LAYERS))))

(define-cstruct _layer-data
  ([cx _float]
   [cy _float]
   [mx _float]
   [my _float]
   [theta _float]
   [mode7-coeff _float]
   [horizon _float]
   [fov _float]))

(define default-layer
  (make-layer-data 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0))

(define-cstruct _sprite-data
  ([dx _float]
   [dy _float]
   [mx _float]
   [my _float]
   [theta _float]   
   [a _float]
   [spr-idx _ushort]
   [pal-idx _ushort]
   [layer _byte]
   [r _byte]
   [g _byte]
   [b _byte]))

(define (tree-for f t)
  (match t
    [(or #f (? void?) '()) (void)]
    [(cons a d)
     (tree-for f a)
     (tree-for f d)]
    [_
     (f t)]))

(define tree/c
  any/c)

(define layer-vector/c
  (apply vector/c (make-list 8 (or/c false/c layer-data?))))

(define-syntax-rule (backend/c (addl-input ...) output)
  (-> layer-vector/c tree/c addl-input ... output))

(define draw!/c (backend/c () void?))
(define render/c (backend/c () bytes?))

(define (stage-backend/c output/c)
  (-> compiled-sprite-db?
      exact-nonnegative-integer?
      exact-nonnegative-integer?
      output/c))

(provide
 (all-defined-out))
