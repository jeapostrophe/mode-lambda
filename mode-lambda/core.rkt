#lang racket/base
(require racket/match
         racket/contract/base
         (except-in ffi/unsafe ->))

(define palette-depth 16)

(struct compiled-sprite-db 
  (atlas-size atlas-bs spr->idx idx->w*h*tx*ty pal-size pal-bs pal->idx))

(define layers 8)
(define layer/c
  (and/c byte? (between/c 0 (sub1 layers))))

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

(define draw!/c
  (-> tree/c void?))
(define render/c
  (-> tree/c bytes?))

(define (stage-backend/c output/c)
  (-> compiled-sprite-db?
      exact-nonnegative-integer?
      exact-nonnegative-integer?
      output/c))

(provide
 (all-defined-out))
