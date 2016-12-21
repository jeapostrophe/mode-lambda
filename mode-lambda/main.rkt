#lang racket/base
(require racket/match
         racket/contract/base
         (except-in ffi/unsafe ->)
         file/gunzip
         racket/file
         mode-lambda/color
         "core.rkt")

(define (sprite-idx csd spr)
  (hash-ref (compiled-sprite-db-spr->idx csd) spr #f))
(define (palette-idx csd pal)
  (hash-ref (compiled-sprite-db-pal->idx csd) pal #f))

(define (sprite-width csd idx)
  (match-define (vector w h tx ty)
    (vector-ref (compiled-sprite-db-idx->w*h*tx*ty csd) idx))
  w)
(define (sprite-height csd idx)
  (match-define (vector w h tx ty)
    (vector-ref (compiled-sprite-db-idx->w*h*tx*ty csd) idx))
  h)

(define (read/bytes/gunzip bs)
  (define-values (read-v write-v-bs) (make-pipe))
  (define read-v-bs/gz (open-input-bytes bs))
  (gunzip-through-ports read-v-bs/gz write-v-bs)
  (close-output-port write-v-bs)
  (read read-v))

(define (load-csd/bs bs)
  (match-define
    (vector 1 spr->idx idx->w*h*tx*ty pal->idx
            atlas-bs atlas-size
            pal-bs pal-size)
    (read/bytes/gunzip bs))
  (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                      pal-size pal-bs pal->idx))

(define (load-csd p)
  (load-csd/bs (file->bytes (build-path p "csd.rktd.gz"))))

(define (sprite cx cy spr-idx
                #:layer [layer 0]
                #:r [r 0]
                #:g [g 0]
                #:b [b 0]
                #:a [a 1.0]
                #:pal-idx [pal-idx 0]
                #:m [m 1.0]
                #:mx [mx m]
                #:my [my m]
                #:theta [theta 0.0])
  (make-sprite-data cx cy mx my theta a spr-idx pal-idx layer r g b))

(define (layer cx cy
               #:hw [hw +inf.0]
               #:hh [hh +inf.0]
               #:wrap-x? [wrap-x? #f]
               #:wrap-y? [wrap-y? #f]
               #:mx [mx 1.0]
               #:my [my 1.0]
               #:theta [theta 0.0]
               #:mode7 [mode7-coeff 0.0]
               #:horizon [horizon 0.0]
               #:fov [fov 1.0])
  (make-layer-data cx cy hw hh mx my theta mode7-coeff horizon fov wrap-x? wrap-y?))

(provide
 (contract-out
  [PALETTE-DEPTH
   exact-nonnegative-integer?]
  [compiled-sprite-db?
   (-> any/c
       boolean?)]
  [load-csd/bs
   (-> bytes?
       compiled-sprite-db?)]
  [load-csd
   (-> path-string?
       compiled-sprite-db?)]
  [sprite-idx
   (-> compiled-sprite-db? symbol?
       (or/c #f ushort?))]
  [palette-idx
   (-> compiled-sprite-db? symbol?
       (or/c #f ushort?))]
  [sprite-width
   (-> compiled-sprite-db? ushort?
       ushort?)]
  [sprite-height
   (-> compiled-sprite-db? ushort?
       ushort?)]
  [sprite
   (->* (flonum? flonum? ushort?)
        (#:layer
         layer/c
         #:r byte? #:g byte? #:b byte? #:a flonum?
         #:pal-idx ushort?
         #:m flonum? #:mx flonum? #:my flonum?
         #:theta flonum?)
        sprite-data?)]
  [sprite-data? (-> any/c boolean?)]
  [sprite-data-dx (-> sprite-data? flonum?)]
  [sprite-data-dy (-> sprite-data? flonum?)]
  [sprite-data-mx (-> sprite-data? flonum?)]
  [sprite-data-my (-> sprite-data? flonum?)]
  [sprite-data-spr (-> sprite-data? ushort?)]
  [sprite-data-layer (-> sprite-data? byte?)]
  [layer
   (->* (flonum? flonum?)
        (#:hw
         flonum?
         #:hh flonum?
         #:wrap-x? boolean?
         #:wrap-y? boolean?
         #:mx flonum?
         #:my flonum?
         #:theta flonum?
         #:mode7 flonum?
         #:horizon flonum?
         #:fov flonum?)
        layer-data?)]))
