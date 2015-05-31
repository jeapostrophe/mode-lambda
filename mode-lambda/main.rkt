#lang racket/base
(require racket/match
         racket/contract/base
         (except-in ffi/unsafe ->)
         mode-lambda/color
         "core.rkt")

(struct sprite-db (sprite-loaders-box palettes))

(define (make-sprite-db)
  (sprite-db (box null) (make-hasheq)))

(define (in->bm in)
  (local-require racket/draw
                 racket/class)
  (define bm
    (if (or (path-string? in) (input-port? in))
        (read-bitmap in)
        in))
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define bs (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h bs)
  (values w h bs))

(define (add-sprite! sd load)
  (match-define (sprite-db ls-b _) sd)
  (set-box! ls-b (cons load (unbox ls-b)))
  (void))
(define (add-sprite!/bm sd name load-bm #:palette [pal #f])
  (define (load)
    (define in (load-bm))
    (define-values (w h bs) (in->bm in))
    (vector name pal w h bs))
  (add-sprite! sd load))
(define (add-sprite!/file sd name p #:palette [pal #f])
  (add-sprite!/bm sd name (λ () p) #:palette pal))
(define (add-sprite!/value sd name v #:palette [pal #f])
  (local-require file/convertible)
  (add-sprite!/bm
   sd name
   #:palette pal
   (λ ()
     (define bs (convert v 'png-bytes))
     (define ip (open-input-bytes bs))
     ip)))

(define (add-palette! sd n pal)
  (define pals (sprite-db-palettes sd))
  (hash-set! pals n pal))
(define (add-palette!/file sd n p)
  (define-values (w h bs) (in->bm p))
  (define pal
    (for/list ([i (in-range PALETTE-DEPTH)])
      (define start (* 4 i))
      (subbytes bs start (+ start 4))))
  (add-palette! sd n pal))

(define (ushort? x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x 65535)))
(define (compile-sprite-db sd)
  (local-require "korf-bin.rkt")
  (match-define (sprite-db ls-b palettes) sd)

  (define pal-size (add1 (hash-count palettes)))
  (define pal-bs (make-bytes (* pal-size PALETTE-DEPTH 4)))
  (define pal->idx (make-hasheq))
  (for ([(n p) (in-hash palettes)]
        [y (in-naturals 1)])
    (for ([c (in-list p)]
          [x (in-range PALETTE-DEPTH)])
      (bytes-copy! pal-bs (+ (* 4 PALETTE-DEPTH y) (* 4 x)) c))
    (hash-set! pal->idx n y))

  (define ss (map (λ (l) (l)) (unbox ls-b)))

  (define-values (atlas-size places)
    (pack (λ (s) (vector-ref s 2))
          (λ (s) (vector-ref s 3))
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
    (match-define (placement tx ty (vector spr pal w h bs)) pl)
    (match pal
      [#f
       (for ([by (in-range h)])
         (bytes-copy! atlas-bs
                      (+ (* 4 atlas-size (+ ty by))
                         (* 4 tx))
                      bs
                      (* 4 w by) (* 4 w (add1 by))))]
      [_
       (define idx
         (hash-ref pal->idx pal
                   (λ () (error 'compile-sprite-db
                                "Unknown palette ~v for sprite ~v"
                                pal spr))))
       (define lookup
         (for/hash ([i (in-range PALETTE-DEPTH)])
           (define start (+ (* 4 PALETTE-DEPTH idx) (* 4 i)))
           (values (subbytes pal-bs start (+ start 4)) i)))
       (define from-sprite (make-bytes 4))
       (define o-alpha 0)
       (define   o-red 1)
       (define o-green 2)
       (define  o-blue 3)
       (define to-atlas (bytes 255 0 0 0))
       (for* ([bx (in-range w)]
              [by (in-range h)])
         (define start (+ (* 4 w by) (* 4 bx)))
         (bytes-copy! from-sprite 0
                      bs
                      start (+ start 4))
         (define which
           (hash-ref lookup from-sprite
                     (λ ()
                       (error 'compile-sprite-db
                              "Sprite ~v has a color ~v not from palette ~v: ~v\n"
                              spr (bytes->list from-sprite) pal
                              (map bytes->list (hash-keys lookup)))
                       0)))
         (bytes-set! to-atlas o-green
                     ;; This used to just be WHICH, but it can't
                     ;; because the numbers are too small to be
                     ;; represented as floats.
                     (* 14 which))
         (bytes-set! to-atlas o-blue
                     (inexact->exact (floor (* 255 (/ which PALETTE-DEPTH)))))
         (bytes-copy! atlas-bs
                      (+ (* 4 atlas-size (+ ty by))
                         (* 4 (+ tx bx)))
                      to-atlas))])
    (hash-set! spr->idx spr pi)
    (vector-set! idx->w*h*tx*ty pi (vector w h tx ty)))

  (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                      pal-size pal-bs pal->idx))

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

(require file/gunzip
         file/gzip)
(define (write-to-file/gzip v p)
  (define-values (read-v-bs write-v) (make-pipe))
  (write v write-v)
  (close-output-port write-v)
  (call-with-output-file
    p #:exists 'replace
    (λ (write-v-bs/gz)
      (gzip-through-ports read-v-bs write-v-bs/gz #f (current-seconds)))))
(define (file->value/gunzip p)
  (define-values (read-v write-v-bs) (make-pipe))
  (call-with-input-file p
    (λ (read-v-bs/gz)
      (gunzip-through-ports read-v-bs/gz write-v-bs)))
  (close-output-port write-v-bs)
  (read read-v))

(define (save-csd! csd p)
  (local-require racket/file)
  (make-directory* p)
  (match-define
    (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                        pal-size pal-bs pal->idx)
    csd)
  (write-png-bytes! atlas-bs atlas-size atlas-size (build-path p "sprites.png"))
  (write-png-bytes! pal-bs PALETTE-DEPTH pal-size (build-path p "palettes.png"))
  (write-to-file/gzip
   (vector spr->idx idx->w*h*tx*ty pal->idx)
   (build-path p "csd.rktd.gz"))
  (void))
(define (load-csd p)
  (local-require racket/file)
  (define-values (atlas-size _atlas-size atlas-bs)
    (read-img-bytes (build-path p "sprites.png")))
  (match-define (vector spr->idx idx->w*h*tx*ty pal->idx)
    (file->value/gunzip (build-path p "csd.rktd.gz")))
  (define-values (_pal-depth pal-size pal-bs)
    (read-img-bytes (build-path p "palettes.png")))
  (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                      pal-size pal-bs pal->idx))

(define (sprite dx dy spr-idx
                #:layer [layer 0]
                #:r [r 0]
                #:g [g 0]
                #:b [b 0]
                #:a [a 1.0]
                #:pal-idx [pal-idx 0]
                #:mx [mx 1.0]
                #:my [my 1.0]
                #:theta [theta 0.0])
  (make-sprite-data dx dy mx my theta a spr-idx pal-idx layer r g b))

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

(define (sprite-attributes? x)
  (match x
    [(vector n pal w h bs)
     (and (symbol? n)
          (or (not pal) (symbol? pal))
          (ushort? w)
          (ushort? h)
          (= (bytes-length bs) (* 4 w h)))]
    [_
     #f]))

(provide
 (contract-out
  [LAYERS
   exact-nonnegative-integer?]
  [PALETTE-DEPTH
   exact-nonnegative-integer?]
  [default-layer-config
    layer-vector/c]
  [make-sprite-db
   (-> sprite-db?)]
  [sprite-db?
   (-> any/c boolean?)]
  [sprite-attributes?
   (-> any/c boolean?)]
  [add-sprite!
   (-> sprite-db? (-> sprite-attributes?)
       void?)]
  [add-sprite!/bm
   (->* (sprite-db? symbol?
                    (-> (or/c path-string? input-port?
                              (let ()
                                (local-require racket/class
                                               racket/gui/base)
                                (is-a?/c bitmap%)))))
        (#:palette (or/c #f symbol?))
        void?)]
  [add-sprite!/file
   (->* (sprite-db? symbol? path-string?)
        (#:palette (or/c #f symbol?))
        void?)]
  [add-sprite!/value
   (->* (sprite-db?
         symbol?
         (let () (local-require file/convertible)
              convertible?))
        (#:palette (or/c #f symbol?))
        void?)]
  [add-palette!
   (-> sprite-db?
       symbol?
       (listof color?)
       void?)]
  [add-palette!/file
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
         #:mx flonum? #:my flonum?
         #:theta flonum?)
        any/c)]
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
