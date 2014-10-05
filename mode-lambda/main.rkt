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

(define (compile-sprite-db sd)
  (local-require "korf-bin.rkt")
  (match-define (sprite-db ls-b) sd)
  (define ss (map (λ (l) (l)) (unbox ls-b)))

  (define-values (atlas-size places)
    (pack (λ (s) (vector-ref s 1))
          (λ (s) (vector-ref s 2))
          ss))
  (define how-many-places (length places))
  (define atlas-bs (make-bytes (* atlas-size atlas-size 4)))
  (define spr->idx (make-hasheq))
  (define idx->w*h*tx*ty (make-vector how-many-places #f))

  (for ([pl (in-list places)]
        [pi (in-naturals)])
    (match-define (placement tx ty (vector spr w h bs)) pl)
    (for ([by (in-range h)])
      (bytes-copy! atlas-bs
                   (+ (* 4 atlas-size (+ ty by))
                      (* 4 tx))
                   bs
                   (* 4 w by) (* 4 w (add1 by))))
    (hash-set! spr->idx spr pi)
    (vector-set! idx->w*h*tx*ty pi (vector w h tx ty)))

  (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty))

(define (sprite-idx csd spr)
  (hash-ref (compiled-sprite-db-spr->idx csd) spr #f))

(define (sprite-hw csd spr)
  #f)
(define (sprite-hh csd spr)
  #f)

(define (save-csd! csd p)
  (local-require racket/file)
  (make-directory* p)
  (match-define (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty) csd)
  (let ()
    (local-require racket/draw
                   racket/class)
    (define root-bm
      (make-bitmap atlas-size atlas-size))
    (send root-bm set-argb-pixels 0 0 atlas-size atlas-size atlas-bs)
    (send root-bm save-file (build-path p "atlas.png") 'png 100 #:unscaled? #t))
  ;; xxx save the other things too
  (void))
(define (load-csd p)
  #f)

(define (sprite dx dy r g b a spr-idx pal-idx mx my theta)
  (make-sprite-data dx dy mx my theta spr-idx pal-idx r g b a))

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
  [sprite-db-add!/file
   (-> sprite-db? symbol? path?
       void?)]
  [compile-sprite-db
   (-> sprite-db?
       compiled-sprite-db?)]
  [save-csd!
   (-> compiled-sprite-db? path-string?
       void?)]
  [sprite-idx
   (-> compiled-sprite-db? symbol?
       (or/c #f exact-nonnegative-integer?))]
  [sprite
   (-> flonum? flonum?
       byte? byte? byte? byte?
       exact-nonnegative-integer?
       exact-nonnegative-integer?
       flonum? flonum?
       flonum?
       sprite-data?)]))
