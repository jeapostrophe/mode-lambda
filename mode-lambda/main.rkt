#lang racket/base
(require racket/match
         racket/contract/base
         (except-in ffi/unsafe ->)
         "core.rkt")

(define (make-sprite-db)
  (sprite-db (box null)))

(define (sprite-db-add!/file sd name p)
  (local-require racket/gui/base
                 racket/class)
  (match-define (sprite-db ls-b) sd)
  (define (load)
    (define bm (read-bitmap p))
    (define w (send bm get-width))
    (define h (send bm get-height))
    (define bs (make-bytes (* w h 4)))
    (send bm get-argb-pixels 0 0 w h bs)
    (vector name w h bs))
  (set-box! ls-b (cons load (unbox ls-b)))
  (void))
;; xxx palettes
;; xxx read from pict, htdp/image, etc

(define (ushort? x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x 65535)))
(define (compile-sprite-db sd)
  (local-require "korf-bin.rkt")
  (match-define (sprite-db ls-b) sd)
  (define ss (map (λ (l) (l)) (unbox ls-b)))

  (define-values (atlas-size places)
    (pack (λ (s) (vector-ref s 1))
          (λ (s) (vector-ref s 2))
          ss))
  (define how-many-places (add1 (length places)))
  (unless (ushort? how-many-places)
    (error 'mode-lambda "Maximum sprite count overreached"))
  (define atlas-bs (make-bytes (* atlas-size atlas-size 4)))
  (define spr->idx (make-hasheq))
  (define idx->w*h*tx*ty (make-vector how-many-places #f))

  (vector-set! idx->w*h*tx*ty 0 (vector 0 0 0 0))
  (for ([pl (in-list places)]
        [pi (in-naturals 1)])
    (match-define (placement tx ty (vector spr w h bs)) pl)
    (for ([by (in-range h)])
      (bytes-copy! atlas-bs
                   (+ (* 4 atlas-size (+ ty by))
                      (* 4 tx))
                   bs
                   (* 4 w by) (* 4 w (add1 by))))
    (hash-set! spr->idx spr pi)
    (vector-set! idx->w*h*tx*ty pi (vector w h tx ty)))

  (define pal-size 1)
  (define pal-bs (make-bytes (* pal-size palette-depth 4)))
  (define pal->idx (make-hasheq))

  (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                      pal-size pal-bs pal->idx))

(define (sprite-idx csd spr)
  (hash-ref (compiled-sprite-db-spr->idx csd) spr #f))
(define (pal-idx csd pal)
  (hash-ref (compiled-sprite-db-pal->idx csd) pal #f))

(define (write-png-bytes! bs w h p)
  (local-require racket/draw
                 racket/class)
  (define root-bm (make-bitmap w h))
  (send root-bm set-argb-pixels 0 0 w h bs)
  (send root-bm save-file p 'png 100 #:unscaled? #t))
(define (read-img-bytes p)
  (local-require racket/draw
                 racket/class)
  (define atlas-bm (read-bitmap p))
  (define w (send atlas-bm get-width))
  (define h (send atlas-bm get-height))
  (define atlas-bs (make-bytes (* w h 4)))
  (send atlas-bm get-argb-pixels 0 0 w h atlas-bs)
  (values w h atlas-bs))

(define (save-csd! csd p)
  (local-require racket/file)
  (make-directory* p)
  (match-define
   (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                       pal-size pal-bs pal->idx)
   csd)
  (write-png-bytes! atlas-bs atlas-size atlas-size (build-path p "atlas.png"))
  (write-png-bytes! pal-bs palette-depth pal-size (build-path p "palettes.png"))
  (write-to-file spr->idx (build-path p "sprite.index") #:exists 'replace)
  (write-to-file idx->w*h*tx*ty (build-path p "sprite.data") #:exists 'replace)
  (write-to-file pal->idx (build-path p "palette.index") #:exists 'replace)
  (void))
(define (load-csd p)
  (local-require racket/file)
  (define-values (atlas-size _atlas-size atlas-bs)
    (read-img-bytes (build-path p "atlas.png")))
  (define spr->idx (file->value (build-path p "sprite.index")))
  (define idx->w*h*tx*ty (file->value (build-path p "sprite.data")))
  (define pal->idx (file->value (build-path p "palette.index")))
  (define-values (_pal-depth pal-size pal-bs)
    (read-img-bytes (build-path p "palettes.png")))
  (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                      pal-size pal-bs pal->idx))

(define layer/c
  (and/c byte? (between/c 0 7)))
(define (sprite layer dx dy r g b a spr-idx pal-idx mx my theta)
  (make-sprite-data dx dy mx my theta a spr-idx pal-idx layer r g b))

;; xxx add layers... snes had 4 (or maybe 8 because each layer had 2
;; planes), saturn could do 5 scrolling + 2 rotating, 32 could be done
;; in a single opengl shader, so 8? (opengl will make a texture array,
;; and use a shader with two triangles to combine them in a particular
;; order) [i could actually draw all layers at the same time by making
;; the layer part of the sprite!]

(provide
 (contract-out
  [make-sprite-db
   (-> sprite-db?)]
  [sprite-db?
   (-> any/c
       boolean?)]
  [sprite-db-add!/file
   (-> sprite-db? symbol? path-string?
       void?)]
  [compile-sprite-db
   (-> sprite-db?
       compiled-sprite-db?)]
  [compiled-sprite-db?
   (-> any/c
       boolean?)]
  [save-csd!
   (-> compiled-sprite-db? path-string?
       void?)]
  [load-csd
   (-> path-string?
       compiled-sprite-db?)]
  [sprite-idx
   (-> compiled-sprite-db? symbol?
       (or/c #f ushort?))]
  [pal-idx
   (-> compiled-sprite-db? symbol?
       (or/c #f ushort?))]
  [sprite
   (-> layer/c flonum? flonum?
       byte? byte? byte? flonum?
       ushort?
       ushort?
       flonum? flonum?
       flonum?
       sprite-data?)]))
