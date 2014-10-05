#lang racket/base
(require racket/match
         ffi/unsafe)

(struct sprite-db (loaders-box))

(struct compiled-sprite-db (atlas-size atlas-bs spr->idx idx->w*h*tx*ty))

(define-cstruct _sprite-data
  ([dx _float]
   [dy _float]
   [mx _float]
   [my _float]
   [theta _float]
   [spr-idx _short]
   [pal-idx _short]
   [r _byte]
   [g _byte]
   [b _byte]
   [a _byte]))

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
