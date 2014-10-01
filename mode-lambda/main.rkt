#lang racket/base
(require racket/match
         racket/contract/base)

(struct sprite-db (spr->load))
(define (make-sprite-db)
  (sprite-db (make-hasheq)))

(define (sprite-db-add!/file sd name p)
  (match-define (sprite-db s->l) sd)
  (hash-set! s->l name
             (λ ()
               (local-require racket/gui/base
                              racket/class)
               (define bm (read-bitmap p))
               (define w (send bm get-width))
               (define h (send bm get-height))
               (define bs (make-bytes (* w h 4)))
               (send bm get-argb-pixels 0 0 w h bs)
               (vector w h bs)))
  (void))
;; xxx more

(struct compiled-sprite-db (s->w*h*bs))
(define (compile-sprite-db sd)
  (match-define (sprite-db s->l) sd)
  ;; xxx change to compute static-dbs
  (define s->w*h*bs
    (for/hasheq ([(s l) (in-hash s->l)])
      (values s (l))))
  (compiled-sprite-db s->w*h*bs))

(define (sprite-hw csd spr)
  #f)
(define (sprite-hh csd spr)
  #f)

(define (save-csd csd p)
  #f)
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
;; order)

(define (make-draw csd width height)
  (match-define (compiled-sprite-db s->w*h*bs) csd)
  (define root-bs (make-bytes (* 4 width height)))
  (lambda (sprite-tree)
    (local-require racket/math
                   racket/flonum
                   racket/performance-hint
                   racket/unsafe/ops)

    (begin-encourage-inline
     (define ?flvector-ref flvector-ref)
     (define ?flvector-set! flvector-set!)
     (define ?bytes-ref bytes-ref)
     (define ?bytes-set! bytes-set!)
     (define ?flcos cos)
     (define ?flsin sin)
     (define ?fl* *)
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
            (* (?flvector-ref A (3*3mat-offset i k))
               (?flvector-ref v k)))))
       u))

    ;; This whole function has lots of opportunities to be
    ;; optimized. Most of the math, when you unroll and inline all the
    ;; functions, has TONS of common-sub-expressions with no-state
    ;; between them. Similarly, we could make do with a smaller amount
    ;; of memory by sharing matrices/vertexes inside and outside this
    ;; loop. I won't do this, because this is a "reference" software
    ;; renderer.

    ;; xxx We are doing per-poly drawing, but we could use the
    ;; "scanline algorithm": basically, sort the triangles by
    ;; Y-position/X-pos, then do a single iteration over the screen (like
    ;; the iteration of x/y in draw-triangle) and then go through the
    ;; sorted lists of triangles and check out which triangle a point
    ;; is part of and then use its fragment function to compute the
    ;; color. This will be much more like how OpenGL works. Right now
    ;; we are (2*S)*(W*H) because in the worst case each triangle
    ;; could potentially cover the screen. The scanline algorithm
    ;; would be (W*H)*(2*S) because in the worst case every triangle
    ;; is on every line. I think in practice S_i is much smaller than
    ;; W_k*H_k, so scanline should be faster

    ;; XXX in OpenGL bs-* would be a global
    (define (fragment-shader fill! bs bs-w bs-h a r g b tx.0 ty.0)
      (define tx (inexact->exact (floor tx.0)))
      (define ty (inexact->exact (floor ty.0)))
      (define-syntax-rule (define-nc cr nr i r)
        (begin (define cr (pixel-ref bs bs-w bs-h tx ty i))
               (define nr (inexact->exact (round (* cr r))))))
      (define-nc ca na 0 a)
      (define-nc cr nr 1 r)
      (define-nc cg ng 2 g)
      (define-nc cb nb 3 b)
      ;; xxx pal translation goes here

      ;; This "unless" corresponds to discarding non-opaque pixels
      (unless (zero? na)
        (fill! na nr ng nb)))

    ;; bx and ty are like "varying" vertex attributes that are
    ;; interpolated across the triangle. bs* are globals, and argb is
    ;; non-varying
    (struct triangle
      (bs bs-w bs-h a r g b
          v1 tx1 ty1
          v2 tx2 ty2
          v3 tx3 ty3))
    (define (triangle-F-O F O t)
      (match-define
       (triangle bs bs-w bs-h a r g b
                 v1 tx1 ty1
                 v2 tx2 ty2
                 v3 tx3 ty3)
       t)
      (F (?flvector-ref v1 O)
         (?flvector-ref v2 O)
         (?flvector-ref v3 O)))
    (define (triangle-F-y F t) (triangle-F-O F 1 t))
    (define (triangle-min-y t) (triangle-F-y min t))
    (define (triangle-max-y t) (triangle-F-y max t))
    (define (point-in-triangle? t x y)
      (match-define
       (triangle bs bs-w bs-h a r g b
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
      (cond
       [(and (<= 0 λ1 1)
             (<= 0 λ2 1)
             (<= 0 λ3 1))
        (vector λ1 λ2 λ3)]
       [else
        #f]))

    (define (geometry-shader output! s)
      (match-define (sprite-data dx dy r g b a spr pal mx my theta) s)
      (define M
        (3*3mat-mult*
         (2d-rotate (* theta (/ 180.0 pi)))
         (2d-translate dx dy)))
      (match-define (vector spr-w spr-h bs) (hash-ref s->w*h*bs spr))
      (define start-tx 0)
      (define start-ty 0)

      (define hw (* (/ spr-w 2) mx))
      (define hh (* (/ spr-h 2) my))
      (define LU
        (3*3matX3vec-mult M (2d-point (* -1.0 hw) (* +1.0 hh))))
      (define RU
        (3*3matX3vec-mult M (2d-point (* +1.0 hw) (* +1.0 hh))))
      (define LL
        (3*3matX3vec-mult M (2d-point (* -1.0 hw) (* -1.0 hh))))
      (define RL
        (3*3matX3vec-mult M (2d-point (* +1.0 hw) (* -1.0 hh))))

      (output!
       (triangle bs spr-w spr-h a r g b
                 LU start-tx (+ start-ty spr-h)
                 RU (+ start-tx spr-w) (+ start-ty spr-h)
                 LL start-tx start-ty))
      (output!
       (triangle bs spr-w spr-h a r g b
                 LL start-tx start-ty
                 RU (+ start-tx spr-w) (+ start-ty spr-h)
                 RL (+ start-tx spr-w) start-tx)))

    (define y->triangles (make-vector height null))
    (define (output! t)
      (for ([y (in-range (max 0
                              (inexact->exact (floor (triangle-min-y t))))
                         (min height
                              (inexact->exact (ceiling (triangle-max-y t)))))])
        (vector-set! y->triangles y 
                     (cons t (vector-ref y->triangles y)))))
    (tree-for (λ (s) (geometry-shader output! s)) sprite-tree)

    ;; Clear the screen
    (bytes-fill! root-bs 0)

    (define (fill-screen!)
      (for* ([y (in-range 0 height)]
             [x (in-range 0 width)])
        (let/ec drew
          (for ([t (in-list (vector-ref y->triangles y))])
            (match (point-in-triangle? t x y)
              [(vector λ1 λ2 λ3)
               (match-define
                (triangle bs bs-w bs-h a r g b
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
               (fragment-shader fill! bs bs-w bs-h a r g b tx ty)]
              [#f
               (void)])))))

    (fill-screen!)

    ;; XXX Add a white background to easily tell the difference
    ;; between border and background in Preview
    (for* ([x (in-range width)]
           [y (in-range height)]
           #:when (= 0 (pixel-ref root-bs width height x y 0)))
      (pixel-set! root-bs width height x y 0 255)
      (pixel-set! root-bs width height x y 1 255)
      (pixel-set! root-bs width height x y 2 255)
      (pixel-set! root-bs width height x y 3 255))

    (let ()
      (local-require racket/draw
                     racket/class)
      (define root-bm
        (make-bitmap width height))
      (send root-bm set-argb-pixels 0 0 width height root-bs)
      #;(eprintf "WROTE ~v\n" root-bs)
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
  [sprite
   (-> flonum? flonum?
       flonum? flonum? flonum? flonum?
       symbol? ;; XXX replace with compiled form
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
