#lang racket/base
(require racket/match
         racket/contract/base)

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
;; xxx more

;; xxx unsafe/inline
(define (pixel-ref bs w h bx by i)
  (bytes-ref bs (+ (* 4 w by) (* 4 bx) i)))
(define (pixel-set! bs w h bx by i v)
  (bytes-set! bs (+ (* 4 w by) (* 4 bx) i) v))

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
    (for* ([bx (in-range w)]
           [by (in-range h)]
           [off (in-range 4)])
      ;; xxx use bytes-copy! or make bytes-block-copy!
      (pixel-set! atlas-bs atlas-size atlas-size
                  (+ tx bx) (+ ty by) off
                  (pixel-ref bs w h bx by off)))
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

(struct sprite-data (dx dy r g b a spr pal mx my theta))
(define (sprite dx dy r g b a spr pal mx my theta)
  (sprite-data dx dy r g b a spr pal mx my theta))

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
                 racket/flonum
                 racket/performance-hint
                 racket/unsafe/ops
                 data/2d-hash)

  (begin-encourage-inline
   (define ?flvector-ref flvector-ref)
   (define ?flvector-set! flvector-set!)
   (define ?bytes-ref bytes-ref)
   (define ?bytes-set! bytes-set!)
   (define ?flcos cos)
   (define ?flsin sin)
   (define ?fl* *)
   (define ?fl+ +)
   (define ?fl- -)
   (define (pixel-ref bs w h bx by i)
     (?bytes-ref bs (+ (* 4 w by) (* 4 bx) i)))
   (define (pixel-set! bs w h bx by i v)
     (?bytes-set! bs (+ (* 4 w by) (* 4 bx) i) v))
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
   (define (3*3mat-identity)
     (3*3mat 1.0 0.0 0.0
             0.0 1.0 0.0
             0.0 0.0 1.0))
   (define (2d-translate dx dy)
     (3*3mat 1.0 0.0 dx
             0.0 1.0 dy
             0.0 0.0 1.0))
   (define (2d-rotate theta)
     (define cost (?flcos theta))
     (define sint (?flsin theta))
     (3*3mat cost sint 0.0
             (?fl* -1 sint) cost 0.0
             0.0 0.0 1.0))
   (define (3*3mat-offset i j)
     (+ (* 3 i) j))
   (define (3*3mat-mult A B)
     (define C (3*3mat-zero))
     (for* ([i (in-range 3)]
            [j (in-range 3)])
       (?flvector-set!
        C (3*3mat-offset i j)
        (for/sum ([k (in-range 3)])
          (* (?flvector-ref A (3*3mat-offset i k))
             (?flvector-ref B (3*3mat-offset k j))))))
     C)
   (define-syntax 3*3mat-mult*
     (syntax-rules ()
       [(_) (3*3mat-identity)]
       [(_ a . more) (3*3mat-mult a (3*3mat-mult* . more))]))
   (define (3vec i0 i1 i2)
     (flvector i0 i1 i2))
   (define (2d-point x y)
     (3vec x y 1.0))
   (define (3vec-zero)
     (3vec 0.0 0.0 0.0))
   (define (3*3matX3vec-mult A v)
     (define u (3vec-zero))
     (for* ([i (in-range 3)])
       (?flvector-set!
        u i
        (for/sum ([k (in-range 3)])
          (?fl* (?flvector-ref A (3*3mat-offset i k))
                (?flvector-ref v k)))))
     u)
   (define (3vec-mult*add λA A λB B C)
     (define u (3vec-zero))
     (for* ([i (in-range 3)])
       (?flvector-set!
        u i
        (?fl+ (?fl* λA (?flvector-ref A i))
              (?fl* λB (?flvector-ref B i))
              (?flvector-ref C i))))
     u))


  ;; XXX separate into verticex-data & attributes
  ;; XXX use cstructs for attributes

  ;; bx and ty are like "varying" vertex attributes that are
  ;; interpolated across the triangle. bs* are globals, and argb is
  ;; non-varying
  (struct triangle
    (a r g b
       v1 tx1 ty1
       v2 tx2 ty2
       v3 tx3 ty3))
  (define (triangle-F-O F O t)
    (match-define
     (triangle a r g b
               v1 tx1 ty1
               v2 tx2 ty2
               v3 tx3 ty3)
     t)
    (F (?flvector-ref v1 O)
       (?flvector-ref v2 O)
       (?flvector-ref v3 O)))
  (define (triangle-F-y F t) (triangle-F-O F 1 t))
  (define (triangle-min-y t) (triangle-F-y min t))
  (define (triangle-min-x t) (triangle-F-O min 0 t))
  (define (triangle-max-y t) (triangle-F-y max t))
  (define (triangle-max-x t) (triangle-F-O max 0 t))
  (define (when-point-in-triangle t x y f)
    (match-define
     (triangle a r g b
               v1 tx1 ty1
               v2 tx2 ty2
               v3 tx3 ty3)
     t)
    (define x1 (?flvector-ref v1 0))
    (define y1 (?flvector-ref v1 1))
    (define x2 (?flvector-ref v2 0))
    (define y2 (?flvector-ref v2 1))
    (define x3 (?flvector-ref v3 0))
    (define y3 (?flvector-ref v3 1))
    (define min-x (min x1 x2 x3))
    (define max-x (max x1 x2 x3))
    (when (<= min-x x max-x)
      ;; Compute the Barycentric coordinates
      (define detT
        (+ (* (- y2 y3) (- x1 x3))
           (* (- x3 x2) (- y1 y3))))
      (define λ1
        (/ (+ (* (- y2 y3) (- x x3))
              (* (- x3 x2) (- y y3)))
           detT))
      (define λ2
        (/ (+ (* (- y3 y1) (- x x3))
              (* (- x1 x3) (- y y3)))
           detT))
      (define λ3
        (- 1 λ1 λ2))
      ;; This condition is when the point is actually in the
      ;; triangle.
      (when (and (<= 0 λ1 1)
                 (<= 0 λ2 1)
                 (<= 0 λ3 1))
        (f λ1 λ2 λ3))))

  (match-define (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty) csd)
  (define root-bs (make-bytes (* 4 width height)))
  (define tri-hash (make-2d-hash width height))
  (lambda (sprite-tree)
    ;; This whole function has lots of opportunities to be
    ;; optimized. Most of the math, when you unroll and inline all the
    ;; functions, has TONS of common-sub-expressions with no-state
    ;; between them. Similarly, we could make do with a smaller amount
    ;; of memory by sharing matrices/vertexes inside and outside this
    ;; loop. I won't do this, because this is a "reference" software
    ;; renderer.

    (define (fragment-shader fill! a r g b tx.0 ty.0)
      (define tx (inexact->exact (floor tx.0)))
      (define ty (inexact->exact (floor ty.0)))
      (define-syntax-rule (define-nc cr nr i r)
        (begin (define cr (pixel-ref atlas-bs atlas-size atlas-size tx ty i))
               (define nr (inexact->exact (round (* cr r))))))
      (define-nc ca na 0 a)
      (define-nc cr nr 1 r)
      (define-nc cg ng 2 g)
      (define-nc cb nb 3 b)
      ;; xxx pal translation goes here

      ;; This "unless" corresponds to discarding non-opaque pixels
      (unless (zero? na)
        (fill! na nr ng nb)))

    (define (geometry-shader output! s)
      (match-define (sprite-data dx dy r g b a spr pal mx my theta) s)
      (define M
        (3*3mat-mult
         (2d-translate dx dy)
         (2d-rotate theta)))
      (define spr-idx (hash-ref spr->idx spr))
      (match-define (vector spr-w spr-h start-tx start-ty)
                    (vector-ref idx->w*h*tx*ty spr-idx))

      (define hw (* (/ spr-w 2) mx))
      (define hh (* (/ spr-h 2) my))
      (define X (3*3matX3vec-mult M (3vec hw 0.0 0.0)))
      (define Y (3*3matX3vec-mult M (3vec 0.0 hh 0.0)))
      (define Z (3*3matX3vec-mult M (3vec 0.0 0.0 1.0)))

      (define LU (3vec-mult*add -1.0 X +1.0 Y Z))
      (define RU (3vec-mult*add +1.0 X +1.0 Y Z))
      (define LL (3vec-mult*add -1.0 X -1.0 Y Z))
      (define RL (3vec-mult*add +1.0 X -1.0 Y Z))

      (define spr-last-x (sub1 spr-w))
      (define spr-last-y (sub1 spr-h))

      (output!
       (triangle a r g b
                 LU start-tx (+ start-ty spr-last-y)
                 RU (+ start-tx spr-last-x) (+ start-ty spr-last-y)
                 LL start-tx start-ty))
      (output!
       (triangle a r g b
                 LL start-tx start-ty
                 RU (+ start-tx spr-last-x) (+ start-ty spr-last-y)
                 RL (+ start-tx spr-last-x) start-tx)))

    (define (output! t)
      (2d-hash-add! tri-hash
                    (max 0
                         (inexact->exact (floor (triangle-min-x t))))
                    (min (sub1 width)
                         (inexact->exact (ceiling (triangle-max-x t))))
                    (max 0
                         (inexact->exact (floor (triangle-min-y t))))
                    (min (sub1 height)
                         (inexact->exact (ceiling (triangle-max-y t))))
                    t))
    (tree-for (λ (s) (geometry-shader output! s)) sprite-tree)

    ;; Clear the screen
    (bytes-fill! root-bs 0)
    ;; Fill the screen
    (for* ([xb (in-range 0 (2d-hash-x-blocks tri-hash))]
           [yb (in-range 0 (2d-hash-y-blocks tri-hash))])
      (define tris (2d-hash-block-ref tri-hash xb yb))
      (unless (null? tris)
        (for* ([x (in-range (2d-hash-x-block-min tri-hash xb)
                            (min width (2d-hash-x-block-max tri-hash xb)))]
               [y (in-range (2d-hash-y-block-min tri-hash yb)
                            (min height (2d-hash-y-block-max tri-hash yb)))])
          (let/ec drew
            (for ([t (in-list tris)])
              (when-point-in-triangle
               t x y
               (λ (λ1 λ2 λ3)
                 (match-define
                  (triangle a r g b
                            v1 tx1 ty1
                            v2 tx2 ty2
                            v3 tx3 ty3)
                  t)
                 (define tx (+ (* λ1 tx1) (* λ2 tx2) (* λ3 tx3)))
                 (define ty (+ (* λ1 ty1) (* λ2 ty2) (* λ3 ty3)))
                 (define (fill! na nr ng nb)
                   (pixel-set! root-bs width height x y 0 na)
                   (pixel-set! root-bs width height x y 1 nr)
                   (pixel-set! root-bs width height x y 2 ng)
                   (pixel-set! root-bs width height x y 3 nb)
                   ;; This is like a "depth" test. If the fragment drew
                   ;; anything, then skip the rest of the triangles
                   (drew))
                 (fragment-shader fill! a r g b tx ty))))))))

    (2d-hash-clear! tri-hash)

    (let ()
      (local-require racket/draw
                     racket/class)
      (define root-bm
        (make-bitmap width height))
      (send root-bm set-argb-pixels 0 0 width height root-bs)
      (send root-bm save-file "lambda.png" 'png 100 #:unscaled? #t))
    (void)))

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
           void?))]))
