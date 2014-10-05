#lang racket/base
(require racket/match
         racket/contract/base
         (except-in ffi/unsafe ->))

(struct sprite-db (loaders-box))
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

(struct compiled-sprite-db (atlas-size atlas-bs spr->idx idx->w*h*tx*ty))
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

(define-cstruct _sprite-data
  ([dx _float]
   [dy _float]
   ;; xxx change to _byte
   [r _float]
   [g _float]
   [b _float]
   [a _float]
   ;; xxx change to _short
   [spr _symbol]
   ;; xxx change to _short
   [pal _stdbool]
   [mx _float]
   [my _float]
   [theta _float]))
(define (sprite dx dy r g b a spr pal mx my theta)
  (make-sprite-data dx dy r g b a spr pal mx my theta))

(define (tree-for f t)
  (cond
   [(null? t)
    (void)]
   [(pair? t)
    (tree-for f (car t))
    (tree-for f (cdr t))]
   [else
    (f t)]))

;; xxx add layers... snes had 4 (or maybe 8 because each layer had 2
;; planes), saturn could do 5 scrolling + 2 rotating, 32 could be done
;; in a single opengl shader, so 8? (opengl will make a texture array,
;; and use a shader with two triangles to combine them in a particular
;; order) [i could actually draw all layers at the same time by making
;; the layer part of the sprite!]

(define (make-draw csd width height)
  (local-require racket/math
                 racket/fixnum
                 racket/flonum
                 racket/performance-hint
                 racket/unsafe/ops
                 data/2d-hash)

  (begin-encourage-inline
   (define ?flvector-ref unsafe-flvector-ref)
   (define ?flvector-set! unsafe-flvector-set!)
   (define (?flvector-set!* vec . vs)
     (for ([i (in-naturals)]
           [v (in-list vs)])
       (?flvector-set! vec i v)))
   (define ?bytes-ref unsafe-bytes-ref)
   (define ?bytes-set! unsafe-bytes-set!)
   (define (flmin3 x y z) (flmin (flmin x y) z))
   (define (flmax3 x y z) (flmax (flmax x y) z))
   (define (pixel-ref bs w h bx by i)
     (?bytes-ref bs (fx+ (fx* (fx* 4 w) by) (fx+ (fx* 4 bx) i))))
   (define (pixel-set! bs w h bx by i v)
     (?bytes-set! bs (fx+ (fx* (fx* 4 w) by) (fx+ (fx* 4 bx) i)) v))
   (define (3*3mat i00 i01 i02
                   i10 i11 i12
                   i20 i21 i22)
     (flvector i00 i01 i02
               i10 i11 i12
               i20 i21 i22))
   (define (3*3mat-zero)
     (3*3mat 0.0 0.0 0.0
             0.0 0.0 0.0
             0.0 0.0 0.0))
   (define (2d-translate! dx dy M)
     (?flvector-set!* M
                      1.0 0.0 dx
                      0.0 1.0 dy
                      0.0 0.0 1.0))
   (define (2d-rotate! theta M)
     (define cost (flcos theta))
     (define sint (flsin theta))
     (?flvector-set!* M
                      cost sint 0.0
                      (fl* -1.0 sint) cost 0.0
                      0.0 0.0 1.0))
   (define (3*3mat-offset i j)
     (fx+ (fx* 3 i) j))
   (define (3*3mat-mult! A B C)
     (for* ([i (in-range 3)]
            [j (in-range 3)])
       (?flvector-set!
        C (3*3mat-offset i j)
        (for/sum ([k (in-range 3)])
          (fl* (?flvector-ref A (3*3mat-offset i k))
               (?flvector-ref B (3*3mat-offset k j)))))))
   (define (3vec i0 i1 i2)
     (flvector i0 i1 i2))
   (define (3vec-zero)
     (3vec 0.0 0.0 0.0))
   ;; Notice that in these two functions we ignore doing the math for
   ;; the last cell of the vector, because we are specializing for 2d
   ;; points
   (define (3*3matX3vec-mult! A v u)
     (for* ([i (in-range 2)])
       (?flvector-set!
        u i
        (for/sum ([k (in-range 3)])
          (fl* (?flvector-ref A (3*3mat-offset i k))
               (?flvector-ref v k)))))))

  (struct triangle (a r g b
                      v1.x v1.y tx1 ty1
                      v2.x v2.y tx2 ty2
                      v3.x v3.y tx3 ty3
                      detT))
  (define (when-point-in-triangle t x y drew x.0 y.0 draw-triangle!)
    (match-define
     (triangle a r g b
               x1 y1 tx1 ty1
               x2 y2 tx2 ty2
               x3 y3 tx3 ty3
               detT)
     t)
    ;; Compute the Barycentric coordinates
    (define λ1
      (fl/ (fl+ (fl* (fl- y2 y3) (fl- x.0 x3))
                (fl* (fl- x3 x2) (fl- y.0 y3)))
           detT))
    (when (and (fl<= 0.0 λ1) (fl<= λ1 1.0))
      (define λ2
        (fl/ (fl+ (fl* (fl- y3 y1) (fl- x.0 x3))
                  (fl* (fl- x1 x3) (fl- y.0 y3)))
             detT))
      (when (and (fl<= 0.0 λ2) (fl<= λ2 1.0))
        (define λ3
          (fl- (fl- 1.0 λ1) λ2))
        ;; This condition is when the point is actually in the
        ;; triangle.
        (when (and (fl<= 0.0 λ2) (fl<= λ2 1.0))
          (draw-triangle! t x y drew λ1 λ2 λ3)))))

  (match-define (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty) csd)
  (define root-bs (make-bytes (* 4 width height)))
  (define tri-hash (make-2d-hash width height))

  (define T (3*3mat-zero))
  (define R (3*3mat-zero))
  (define M (3*3mat-zero))
  (define X (3vec-zero))
  (define Y (3vec-zero))
  (define Z (3vec-zero))
  (lambda (sprite-tree)
    (define (geometry-shader s)
      (match-define (sprite-data dx dy r g b a spr pal mx my theta) s)
      (2d-translate! dx dy T)
      (2d-rotate! theta R)
      (3*3mat-mult! T R M)
      ;; XXX move this into the sprite-data constructor
      (define spr-idx (hash-ref spr->idx spr))
      (match-define (vector spr-w spr-h tx-left tx-bot)
                    (vector-ref idx->w*h*tx*ty spr-idx))

      (define hw (fl* (fl/ (fx->fl spr-w) 2.0) mx))
      (define hh (fl* (fl/ (fx->fl spr-h) 2.0) my))
      (3*3matX3vec-mult! M (3vec hw 0.0 0.0) X)
      (3*3matX3vec-mult! M (3vec 0.0 hh 0.0) Y)
      (3*3matX3vec-mult! M (3vec 0.0 0.0 1.0) Z)

      (begin-encourage-inline
       (define (combine λX λY i)
         (fl+ (fl+ (fl* λX (?flvector-ref X i))
                   (fl* λY (?flvector-ref Y i)))
              (?flvector-ref Z i)))
       (define (detT x1 y1 x2 y2 x3 y3)
         (fl+ (fl* (fl- y2 y3) (fl- x1 x3))
              (fl* (fl- x3 x2) (fl- y1 y3)))))

      (define LTx (combine -1.0 +1.0 0))
      (define LTy (combine -1.0 +1.0 1))
      (define RTx (combine +1.0 +1.0 0))
      (define RTy (combine +1.0 +1.0 1))
      (define LBx (combine -1.0 -1.0 0))
      (define LBy (combine -1.0 -1.0 1))
      (define RBx (combine +1.0 -1.0 0))
      (define RBy (combine +1.0 -1.0 1))

      (define spr-last-x (fx- spr-w 1))
      (define spr-last-y (fx- spr-h 1))

      (define tx-top (fx+ tx-bot spr-last-y))
      (define tx-right (fx+ tx-left spr-last-x))

      (define tx-top.0 (fx->fl tx-top))
      (define tx-bot.0 (fx->fl tx-bot))
      (define tx-left.0 (fx->fl tx-left))
      (define tx-right.0 (fx->fl tx-right))

      (output!
       (triangle a r g b
                 LTx LTy tx-left.0 tx-top.0
                 RBx RBy tx-right.0 tx-bot.0
                 LBx LBy tx-left.0 tx-bot.0
                 (detT LTx LTy RBx RBy LBx LBy)))
      (output!
       (triangle a r g b
                 LTx LTy tx-left.0 tx-top.0
                 RTx RTy tx-right.0 tx-top.0
                 RBx RBy tx-right.0 tx-bot.0
                 (detT LTx LTy RTx RTy RBx RBy))))

    (define (output! t)
      (match-define
       (triangle a r g b
                 x1 y1 tx1 ty1
                 x2 y2 tx2 ty2
                 x3 y3 tx3 ty3
                 detT)
       t)
      (2d-hash-add! tri-hash
                    (fxmax 0
                           (fl->fx (flfloor (flmin3 x1 x2 x3))))
                    (fxmin (fx- width 1)
                           (fl->fx (flceiling (flmax3 x1 x2 x3))))
                    (fxmax 0
                           (fl->fx (flfloor (flmin3 y1 y2 y3))))
                    (fxmin (fx- height 1)
                           (fl->fx (flceiling (flmax3 y1 y2 y3))))
                    t))
    (tree-for geometry-shader sprite-tree)

    (define (fragment-shader drew x y
                             a r g b
                             tx.0 ty.0)
      (define tx (fl->fx (flfloor tx.0)))
      (define ty (fl->fx (flfloor ty.0)))
      (define-syntax-rule (define-nc cr nr i r)
        (begin (define cr (pixel-ref atlas-bs atlas-size atlas-size tx ty i))
               (define nr (fl->fx (flround (fl* (fx->fl cr) r))))))
      (define-nc ca na 0 a)
      (define-nc cr nr 1 r)
      (define-nc cg ng 2 g)
      (define-nc cb nb 3 b)
      ;; xxx pal translation goes here

      ;; This "unless" corresponds to discarding non-opaque pixels
      (unless (fx= 0 na)
        (fill! drew x y na nr ng nb)))

    ;; Clear the screen
    (bytes-fill! root-bs 0)
    ;; Fill the screen
    (define (fill! drew x y na nr ng nb)
      (pixel-set! root-bs width height x y 0 na)
      (pixel-set! root-bs width height x y 1 nr)
      (pixel-set! root-bs width height x y 2 ng)
      (pixel-set! root-bs width height x y 3 nb)
      ;; This is like a "depth" test. If the fragment drew
      ;; anything, then skip the rest of the triangles
      (drew))
    (define (draw-triangle! t x y drew λ1 λ2 λ3)
      (match-define
       (triangle a r g b
                 _ _ tx1.0 ty1.0
                 _ _ tx2.0 ty2.0
                 _ _ tx3.0 ty3.0
                 _)
       t)
      (define tx (fl+ (fl+ (fl* λ1 tx1.0)
                           (fl* λ2 tx2.0))
                      (fl* λ3 tx3.0)))
      (define ty (fl+ (fl+ (fl* λ1 ty1.0)
                           (fl* λ2 ty2.0))
                      (fl* λ3 ty3.0)))
      (fragment-shader drew x y
                       a r g b
                       tx ty))

    (for* ([xb (in-range 0 (2d-hash-x-blocks tri-hash))]
           [yb (in-range 0 (2d-hash-y-blocks tri-hash))])
      (define tris (2d-hash-block-ref tri-hash xb yb))
      (unless (null? tris)
        (for* ([x (in-range (2d-hash-x-block-min tri-hash xb)
                            (min width (2d-hash-x-block-max tri-hash xb)))]
               [y (in-range (2d-hash-y-block-min tri-hash yb)
                            (min height (2d-hash-y-block-max tri-hash yb)))])
          (define x.0 (fx->fl x))
          (define y.0 (fx->fl y))
          (let/ec drew
            (for ([t (in-list tris)])
              (when-point-in-triangle
               t x y drew x.0 y.0
               draw-triangle!))))))

    (2d-hash-clear! tri-hash)

    root-bs))

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
  [sprite
   (-> flonum? flonum?
       flonum? flonum? flonum? flonum?
       symbol? ;; XXX replace with compiled form (idx)
       #f ;; XXX replace with #f or compiled form
       flonum? flonum?
       flonum?
       sprite-data?)]
  [make-draw
   (-> compiled-sprite-db?
       exact-nonnegative-integer?
       exact-nonnegative-integer?
       (-> any/c
           any))]))
