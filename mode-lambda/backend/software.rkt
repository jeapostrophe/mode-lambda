#lang racket/base
;; improve based on https://github.com/Steve132/uraster or just assume
;; the GL will be used and this is for reference?
(require (for-syntax racket/base
                     (only-in srfi/1 iota))
         data/2d-hash
         racket/contract/base
         racket/require
         (filtered-in (λ (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops)
         (only-in (combine-in racket/fixnum
                              racket/flonum)
                  flvector)
         "../util.rkt"
         racket/match
         racket/math
         racket/performance-hint
         "../core.rkt")

(begin-encourage-inline
 (define-syntax-rule (flvector-set!*i vec [i v] ...)
   (begin (flvector-set! vec i v) ...))
 (define-syntax (flvector-set!* stx)
   (syntax-case stx ()
     [(_ vec v ...)
      (with-syntax ([(i ...) (iota (length (syntax->list #'(v ...))))])
        (syntax/loc stx
          (flvector-set!*i vec [i v] ...)))]))
 (define (fxclamp m x M)
   (fxmax m (fxmin x M)))
 (define (flclamp m x M)
   (flmax m (flmin x M)))
 (define (flabsmax x y)
   (if (fl< (flabs y) (flabs x))
       x
       y))
 (define (flabsmax3 x y z)
   (flabsmax x (flabsmax y z)))
 (define (flmin3 x y z) (flmin (flmin x y) z))
 (define (flmax3 x y z) (flmax (flmax x y) z))
 (define (fl<=3 a b c)
   (and (fl<= a b) (fl<= b c)))
 (define (flmod a n)
   (fl- a (fl* n (flfloor (fl/ a n)))))
 (define (flwrap v c hrng)
   (cond
    [(infinite? hrng)
     v]
    [else
     (fl+ c (fl- (flmod v (fl* 2.0 hrng)) hrng))]))
 (define (pixel-ref bs w h bx by i)
   (bytes-ref bs (fx+ (fx* (fx* 4 w) by) (fx+ (fx* 4 bx) i))))
 (define (pixel-set! bs w h bx by i v)
   (bytes-set! bs (fx+ (fx* (fx* 4 w) by) (fx+ (fx* 4 bx) i)) v))
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
   (flvector-set!* M
                   1.0 0.0 dx
                   0.0 1.0 dy
                   0.0 0.0 1.0))
 (define (2d-rotate! theta M)
   (define cost (flcos theta))
   (define sint (flsin theta))
   (flvector-set!* M
                   cost sint 0.0
                   (fl* -1.0 sint) cost 0.0
                   0.0 0.0 1.0))
 (define (3*3mat-offset i j)
   (fx+ (fx* 3 i) j))
 (define (3*3mat-mult! A B C)
   (for* ([i (in-range 3)]
          [j (in-range 3)])
     (flvector-set!
      C (3*3mat-offset i j)
      (for/sum ([k (in-range 3)])
        (fl* (flvector-ref A (3*3mat-offset i k))
             (flvector-ref B (3*3mat-offset k j)))))))
 (define (3vec i0 i1 i2)
   (flvector i0 i1 i2))
 (define (3vec! i0 i1 i2 V)
   (flvector-set!* V i0 i1 i2))
 (define (3vec-zero)
   (3vec 0.0 0.0 0.0))
 ;; Notice that in this function we ignore doing the math for the last
 ;; cell of the vector, because we are specializing for 2d points
 (define (3*3matX3vec-mult! A v u)
   (for ([i (in-range 2)])
     (flvector-set!
      u i
      (for/sum ([k (in-range 3)])
        (fl* (flvector-ref A (3*3mat-offset i k))
             (flvector-ref v k)))))))

(struct triangle (detT
                  layer pal-idx a r g b
                  v1.x v1.y tx1.0 ty1.0
                  v2.x v2.y tx2.0 ty2.0
                  v3.x v3.y tx3.0 ty3.0))
(define (when-point-in-triangle t x y x.0 y.0 draw-triangle!)
  (match-define (triangle detT _ _ _ _ _ _ x1 y1 _ _ x2 y2 _ _ x3 y3 _ _) t)
  ;; Compute the Barycentric coordinates
  (define λ1
    (fl/ (fl+ (fl* (fl- y2 y3) (fl- x.0 x3))
              (fl* (fl- x3 x2) (fl- y.0 y3)))
         detT))
  (when (fl<=3 0.0 λ1 1.0)
    (define λ2
      (fl/ (fl+ (fl* (fl- y3 y1) (fl- x.0 x3))
                (fl* (fl- x1 x3) (fl- y.0 y3)))
           detT))
    (when (fl<=3 0.0 λ2 1.0)
      (define λ3
        (fl- (fl- 1.0 λ1) λ2))
      ;; If all the conditions hold, the point is actually in the
      ;; triangle.
      (when (fl<=3 0.0 λ3 1.0)
        (draw-triangle! t x y λ1 λ2 λ3)))))

(define (stage-render csd width height)
  (define width.0 (fx->fl width))
  (define hwidth.0 (fl* 0.5 width.0))
  (define height.0 (fx->fl height))
  (define hheight.0 (fl* 0.5 height.0))
  (match-define
   (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                       pal-size pal-bs pal->idx)
   csd)
  (define root-bs-v
    (build-vector LAYERS (λ (i) (make-bytes (* 4 width height)))))
  (define combined-bs
    (make-bytes (* 4 width height)))
  (define tri-hash (make-2d-hash width height))

  (define T (3*3mat-zero))
  (define R (3*3mat-zero))
  (define M0 (3*3mat-zero))
  (define M1 (3*3mat-zero))
  (define M2 (3*3mat-zero))
  (define M (3*3mat-zero))
  (define P (3vec-zero))
  (define X (3vec-zero))
  (define Y (3vec-zero))
  (define Z (3vec-zero))
  (lambda (layer-config sprite-tree)
    ;; The layer multiplications can be used for all vertices and we
    ;; could just upload the correct matrices, one per layer (M2) in
    ;; this code.

    ;; I shouldn't really use a geometry shader in OpenGL, because
    ;; they are slower and this is a simple task, I just have to
    ;; create more vertex data: 4 wraps * 2 triangles * 3 verts * 1
    ;; sprite
    (define (geometry-shader s)
      (local-require ffi/cvector)
      (match-define (sprite-data dx dy mx my theta a.0 spr-idx pal-idx layer 
                                 r g b _ _ _ _)
        (cvector-ref s 0))
      (match-define (layer-data Lcx Lcy Lhw Lhh Lmx Lmy Ltheta
                                _ _ _ wrap-x? wrap-y?)
                    (or (vector-ref layer-config layer)
                        (error 'geometry-shader
                               "Cannot draw on layer ~v without config"
                               layer)))
      ;; M = (Translate(Lcx,Lcy) * (Rotate(Ltheta) * Translate(-Hw,-Hh))) * (Translate(Dx,Dy) * Rotate(Theta))

      ;; First we rotate the sprite
      (2d-rotate! theta R)
      ;; Then we move it to correct place on the layer
      (2d-translate! dx dy T)
      (3*3mat-mult! T R M1)
      ;; Next, we move the whole layer so the center is the (0,0)
      ;; XXX I'm not sure if these should be Lhw and Lhh
      (2d-translate! (fl* -1.0 hwidth.0) (fl* -1.0 hheight.0) T)
      ;; Then, we perform the layer rotation
      (2d-rotate! Ltheta R)
      (3*3mat-mult! R T M0)
      ;; Next, we move the layer back to the new center specified
      (2d-translate! Lcx Lcy T)
      (3*3mat-mult! T M0 M2)
      (3*3mat-mult! M2 M1 M)
      (match-define (vector spr-w spr-h tx-left tx-bot)
                    (vector-ref idx->w*h*tx*ty spr-idx))

      (define spr-w.0 (fx->fl spr-w))
      (define spr-h.0 (fx->fl spr-h))

      (let ()
        (define hw (fl* (fl/ spr-w.0 2.0) (fl* mx Lmx)))
        (3vec! hw 0.0 0.0 P)
        (3*3matX3vec-mult! M P X))
      (let ()
        (define hh (fl* (fl/ spr-h.0 2.0) (fl* my Lmy)))
        (3vec! 0.0 hh 0.0 P)
        (3*3matX3vec-mult! M P Y))
      (let ()
        (3vec! 0.0 0.0 1.0 P)
        (3*3matX3vec-mult! M P Z))

      (begin-encourage-inline
       (define (combine λX λY i)
         (fl+ (fl+ (fl* λX (flvector-ref X i))
                   (fl* λY (flvector-ref Y i)))
              (flvector-ref Z i)))
       (define (detT x1 y1 x2 y2 x3 y3)
         (fl+ (fl* (fl- y2 y3) (fl- x1 x3))
              (fl* (fl- x3 x2) (fl- y1 y3)))))

      (define-syntax-rule (define-LT LTx LTy x y)
        (begin (define LTx (combine x y 0))
               (define LTy (combine x y 1))))

      (define-LT LTx LTy -1.0 +1.0)
      (define-LT RTx RTy +1.0 +1.0)
      (define-LT LBx LBy -1.0 -1.0)
      (define-LT RBx RBy +1.0 -1.0)

      (define spr-last-x (fl- spr-w.0 1.0))
      (define spr-last-y (fl- spr-h.0 1.0))
      (define tx-left.0 (fx->fl tx-left))
      (define tx-bot.0 (fx->fl tx-bot))
      (define tx-top.0 (fl+ tx-bot.0 spr-last-y))
      (define tx-right.0 (fl+ tx-left.0 spr-last-x))

      (define Lw (fl* 2.0 Lhw))
      (define Lh (fl* 2.0 Lhh))
      (define (output-triangle!
               LTx LTy LTtxx LTtxy
               RBx RBy RBtxx RBtxy
               LBx LBy LBtxx LBtxy)
        (define (real-output! LTx LTy RBx RBy LBx LBy)
          (output!
           (triangle (detT LTx LTy RBx RBy LBx LBy)
                     layer pal-idx a.0 r g b
                     LTx LTy LTtxx LTtxy
                     RBx RBy RBtxx RBtxy
                     LBx LBy LBtxx LBtxy)))

        (define-syntax-rule (define-measure measure-x Lw Lcx Lhw)
          (define (measure-x x)
            (fl* Lw (flfloor (fl* (fl/ -1.0 Lw) (fl- x (fl+ Lcx Lhw)))))))
        (define-measure measure-x Lw Lcx Lhw)
        (define-measure measure-y Lh Lcy Lhh)

        ;; I am nervous that this doesn't work for sprites that
        ;; are so big they will end up on both sides of the screen.
        ;;
        ;; xxx update this wrt the gl shader (provide less for
        ;; wrapping sprites)
        (for* ([xs (in-list (list (vector #t (λ (x) 0.0))
                                  (vector wrap-x? measure-x)))]
               [ys (in-list (list (vector #t (λ (y) 0.0))
                                  (vector wrap-y? measure-y)))])
          (match-define (vector apply-x? measure-x) xs)
          (match-define (vector apply-y? measure-y) ys)
          (when (and apply-x? apply-y?)
            (define-syntax-rule (define-adjust adjust-x measure-x LTx RBx LBx)
              (begin (define mx
                       (flabsmax3 (measure-x LTx)
                                  (measure-x RBx)
                                  (measure-x LBx)))
                     (define (adjust-x LTx)
                       (fl+ LTx mx))))
            (define-adjust adjust-x measure-x LTx RBx LBx)
            (define-adjust adjust-y measure-y LTy RBy LBy)
            (real-output! (adjust-x LTx) (adjust-y LTy)
                          (adjust-x RBx) (adjust-y RBy)
                          (adjust-x LBx) (adjust-y LBy)))))

      (output-triangle!
       LTx LTy tx-left.0 tx-top.0
       RBx RBy tx-right.0 tx-bot.0
       LBx LBy tx-left.0 tx-bot.0)
      (output-triangle!
       LTx LTy tx-left.0 tx-top.0
       RTx RTy tx-right.0 tx-top.0
       RBx RBy tx-right.0 tx-bot.0))

    (define Mheight (fx- height 1))
    (define Mwidth (fx- width 1))
    (define (output! t)
      (match-define (triangle _ _ _ _ _ _ _ x1 y1 _ _ x2 y2 _ _ x3 y3 _ _) t)
      (define-syntax-rule (tri-range flfloor flmin3 x1 x2 x3 Mwidth)
        (fxclamp 0
                 (fl->fx (flfloor (flmin3 x1 x2 x3)))
                 Mwidth))
      (2d-hash-add! tri-hash
                    (tri-range flfloor flmin3 x1 x2 x3 Mwidth)
                    (tri-range flceiling flmax3 x1 x2 x3 Mwidth)
                    (tri-range flfloor flmin3 y1 y2 y3 Mheight)
                    (tri-range flceiling flmax3 y1 y2 y3 Mheight)
                    t))
    (tree-for geometry-shader sprite-tree)

    (define (fragment-shader layer x y pal-idx a r g b tx.0 ty.0)
      (define tx (fl->fx (flfloor (fl+ tx.0 0.5))))
      (define ty (fl->fx (flfloor (fl+ ty.0 0.5))))
      (define-syntax-rule (define-cr cr i)
        (define cr
          (if (fx= 0 pal-idx)
              (pixel-ref atlas-bs atlas-size #f tx ty i)
              (pixel-ref pal-bs PALETTE-DEPTH #f
                         (/ (pixel-ref atlas-bs atlas-size #f tx ty 2) 14)
                         pal-idx
                         i))))
      (define-cr ca 0)
      (define-cr cr 1)
      (define-cr cg 2)
      (define-cr cb 3)

      (define-syntax-rule (define-nc* cr nr i r)
        (define nr (fl->fx (flround (fl* (fx->fl cr) r)))))
      (define-syntax-rule (define-nc+ cr nr i r)
        (define nr (fxmin 255 (fx+ cr r))))
      (define-nc* ca na 0 a)
      (define-nc+ cr nr 1 r)
      (define-nc+ cg ng 2 g)
      (define-nc+ cb nb 3 b)

      ;; This "unless" corresponds to discarding non-opaque pixels
      (unless (fx= 0 na)
        (fill! layer x y na nr ng nb)))

    ;; Clear the screen
    (for ([root-bs (in-vector root-bs-v)])
      (bytes-fill! root-bs 0))
    (bytes-fill! combined-bs 0)
    ;; Fill the screen
    (define (fill! layer x y na nr ng nb)
      (define root-bs (vector-ref root-bs-v layer))
      (pixel-set! root-bs width height x y 0 na)
      (pixel-set! root-bs width height x y 1 nr)
      (pixel-set! root-bs width height x y 2 ng)
      (pixel-set! root-bs width height x y 3 nb))
    (define (draw-triangle! t x y λ1 λ2 λ3)
      (match-define
       (triangle _ layer pal-idx a r g b
                 _ _ tx1.0 ty1.0 _ _ tx2.0 ty2.0 _ _ tx3.0 ty3.0)
       t)
      (define tx (fl+ (fl+ (fl* λ1 tx1.0)
                           (fl* λ2 tx2.0))
                      (fl* λ3 tx3.0)))
      (define ty (fl+ (fl+ (fl* λ1 ty1.0)
                           (fl* λ2 ty2.0))
                      (fl* λ3 ty3.0)))
      (fragment-shader layer x y
                       pal-idx a r g b
                       tx ty))

    (define max-xbs (2d-hash-x-blocks tri-hash))
    (define max-ybs (2d-hash-y-blocks tri-hash))
    (for ([xb (in-range 0 max-xbs)])
      (define start-x (2d-hash-x-block-min tri-hash xb))
      (define end-x (fxmin width (2d-hash-x-block-max tri-hash xb)))
      (for ([yb (in-range 0 max-ybs)])
        (define tris (2d-hash-block-ref tri-hash xb yb))
        (unless (null? tris)
          (define start-y (2d-hash-y-block-min tri-hash yb))
          (define end-y (fxmin height (2d-hash-y-block-max tri-hash yb)))
          (for ([x (in-range start-x end-x)])
            (define x.0 (fx->fl x))
            (for ([y (in-range start-y end-y)])
              (define y.0 (fx->fl y))
              (for ([t (in-list tris)])
                (when-point-in-triangle
                 t x y x.0 y.0
                 draw-triangle!)))))))

    (2d-hash-clear! tri-hash)

    (for ([layer-bs (in-vector root-bs-v)]
          [lc (in-vector layer-config)]
          [layer (in-naturals)])
      (match lc
        [#f (void)]
        [(layer-data Lcx Lcy Lhw Lhh Lmx Lmy Ltheta
                     mode7-coeff horizon fov wrap-x? wrap-y?)
         (define (compute-pz ay-horiz)
           (cond
            ;; No Mode7
            [(= mode7-coeff 0.0) 1.0]
            ;; Ceiling
            [(= mode7-coeff 1.0) ay-horiz]
            ;; Floor
            [(= mode7-coeff 2.0) (fl* -1.0 ay-horiz)]
            ;; Cylinder
            [(= mode7-coeff 3.0) (flabs ay-horiz)]
            [else
             0.0]))
         (for ([ay (in-range height)])
           (define py (fx->fl ay))
           (define ay-horiz (fl- horizon py))
           (define pz (compute-pz ay-horiz))
           (unless (fl<= pz 0.0)
             (define-syntax-rule (define-ex ex px ax hwidth.0 Lcx Lhw)
               (begin
                 ;; Y' = ((Y - Yc) * (F/Z)) + Yc
                 (define rx (fl+ (fl* (fl- px hwidth.0) (fl/ fov pz))
                                 hwidth.0))
                 (define ex
                   (cond
                    [(or (infinite? rx) (nan? rx))
                     -1]
                    [else
                     (define wx (flwrap rx Lcx Lhw))
                     (define ix (flround wx))
                     (fl->fx ix)]))))
             (define-ex ey py ay hheight.0 Lcy Lhh)
             (when (and (fx<= 0 ey) (fx< ey height))
               (for ([ax (in-range width)])
                 (define px (fx->fl ax))
                 (pixel-set! combined-bs width height ax ay 0 255)
                 (define-ex ex px ax hwidth.0 Lcx Lhw)
                 (when (and (fx<= 0 ex) (fx< ex width))
                   (define na (pixel-ref layer-bs width height ex ey 0))
                   (define na.0 (fl/ (fx->fl na) 255.0))
                   (define-syntax-rule (combined! off)
                     (begin (define cr (pixel-ref layer-bs width height ex ey off))
                            (define pr (pixel-ref combined-bs width height ax ay off))
                            (pixel-set! combined-bs width height ax ay off
                                        (fl->fx
                                         (flround (fl+ (fl* (fx->fl pr) (fl- 1.0 na.0))
                                                       (fl* na.0 (fx->fl cr))))))))
                   (combined! 1)
                   (combined! 2)
                   (combined! 3))))))]))

    combined-bs))

(require mode-lambda/backend/lib)

(define software-bitmap-path 
  (make-parameter #f))

(define gui-mode 'draw)
(define (stage-draw/dc csd width height)
  (define render (stage-render csd width height))
  (λ (layer-config static-st dynamic-st)
    (define sprite-tree (cons static-st dynamic-st))
    (define bs (render layer-config sprite-tree))
    (define bm (argb-bytes->bitmap width height bs))
    (when (software-bitmap-path)
      (save-bitmap! bm (software-bitmap-path)))
    (λ (w h dc)
      (draw-bitmap! w width h height bm dc))))

(provide
 (contract-out
  [gui-mode symbol?]
  [software-bitmap-path (parameter/c path-string?)]
  [stage-render (stage-backend/c render/c)]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
