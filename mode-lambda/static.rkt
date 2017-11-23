#lang racket/base
(require racket/match
         racket/contract/base
         racket/fixnum
         racket/flonum
         mode-lambda/color
         "core.rkt")

(struct sprite-db (sprite-loaders-box palettes))

(define (make-sprite-db)
  (sprite-db (box null) (make-hasheq)))

(define (in->bm in)
  (local-require racket/class
                 racket/draw)
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
    ;; We add one here to tell the optimization algorithm to give each
    ;; sprite a 1-pixel border. This is to prevent color data bleeding
    ;; across sprites in the atlas texture.
    (pack (λ (s) (add1 (vector-ref s 2)))
          (λ (s) (add1 (vector-ref s 3)))
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


  ; convert to premultiplied alpha
  (for [(i (in-range (/ (bytes-length atlas-bs) 4)))]
    (define idx (* 4 i))
    (define a (bytes-ref atlas-bs idx))

    ; For debugging, force the alpha to be at least 1 so that
    ; when we write-png-bytes!, set-argb-pixels will pay attention
    ; to the rgb values (it ignores them if alpha is 0
    ;(bytes-set! atlas-bs idx (max 1 a))

    (for [(k (in-range 1 4))]
      (define col (bytes-ref atlas-bs (+ idx k)))
      (bytes-set! atlas-bs (+ idx k)
                  (fl->fx (flround (fl/ (fx->fl (* a col)) 255.0))))))

  (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                      pal-size pal-bs pal->idx))

(define (write-png-bytes! bs w h p)
  (local-require racket/class
                 racket/draw)
  (define root-bm (make-bitmap w h))
  (send root-bm set-argb-pixels 0 0 w h bs)
  (send root-bm save-file p 'png 100 #:unscaled? #t))

(require file/gzip)
(define (write-to-file/gzip v p)
  (define-values (read-v-bs write-v) (make-pipe))
  (write v write-v)
  (close-output-port write-v)
  (call-with-output-file
    p #:exists 'replace
    (λ (write-v-bs/gz)
      (gzip-through-ports read-v-bs write-v-bs/gz #f (current-seconds)))))

(define (save-csd! csd p #:debug? [debug? #f])
  (local-require racket/file)
  (make-directory* p)
  (match-define
    (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                        pal-size pal-bs pal->idx)
    csd)
  (when debug?
    (write-png-bytes! atlas-bs atlas-size atlas-size (build-path p "sprites.png"))
    (write-png-bytes! pal-bs PALETTE-DEPTH pal-size (build-path p "palettes.png")))
  (write-to-file/gzip
   (vector 1 spr->idx idx->w*h*tx*ty pal->idx
           atlas-bs atlas-size
           pal-bs pal-size)
   (build-path p "csd.rktd.gz"))
  (void))

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
                                               racket/draw)
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
  [save-csd!
   (->* (compiled-sprite-db? path-string?)
        (#:debug? boolean?)
        void?)]))
