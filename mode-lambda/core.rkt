#lang racket/base
(require racket/match
         ffi/unsafe)

(struct sprite-db (sprite-loaders-box palettes))

(define palette-depth 16)

(struct compiled-sprite-db 
  (atlas-size atlas-bs spr->idx idx->w*h*tx*ty pal-size pal-bs pal->idx))

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
    ['() (void)]
    [(cons a d)
     (tree-for f a)
     (tree-for f d)]
    [_
     (f t)]))

(provide
 (all-defined-out))
