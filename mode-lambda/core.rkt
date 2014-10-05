#lang racket/base
(require racket/contract/base
         (except-in ffi/unsafe ->))

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
  (cond
   [(null? t)
    (void)]
   [(pair? t)
    (tree-for f (car t))
    (tree-for f (cdr t))]
   [else
    (f t)]))

(provide
 ;; xxx
 (all-defined-out))
