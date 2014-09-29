#lang racket/base
(require racket/match
         racket/contract/base)

(struct sprite-db (name->bitmap))
(define (make-sprite-db)
  (sprite-db (make-hasheq)))

(define (sprite-db-add!/file sd name p)
  (match-define (sprite-db n->bm) sd)
  (hash-set! n->bm name
             (λ ()
               (local-require racket/gui/base
                              racket/class)
               (define bm (read-bitmap p))
               (define w (send bm get-width))
               (define h (send bm get-height))
               (define bs (make-bytes (* w h 4)))
               (send bm get-argb-pixels 0 0 w h bs)
               (values w h bs)))
  (void))
;; xxx more

(define (sprite-hw spr)
  #f)
(define (sprite-hh spr)
  #f)

(struct compiled-sprite-db (sd))
(define (compile-sprite-db sd)
  (compiled-sprite-db sd))

(define (save-csd csd p)
  #f)
(define (load-csd p)
  #f)

(struct sprite-data (x y r g b a spr pal mx my theta))
(define (sprite x y r g b a spr pal mx my theta)
  (sprite-data x y r g b a spr pal mx my theta))

(define (tree-for f t)
  (cond
   [(null? t)
    (void)]
   [(pair? t)
    (tree-for f (car t))
    (tree-for f (cdr t))]
   [else
    (f t)]))

(define (pixel-ref bs w h bx by i)
  (bytes-ref bs (+ (* w by) (* 4 bx) i)))

(define (make-draw csd width height)
  (match-define (compiled-sprite-db sd) csd)
  (match-define (sprite-db n->bm) sd)
  (lambda (sprite-tree)
    (local-require racket/class
                   racket/math
                   racket/draw)
    (define root-bm
      (make-platform-bitmap width height))
    (define root-bm-dc (new bitmap-dc% [bitmap root-bm]))
    (define (paint dc)
      (define it (send dc get-transformation))
      (define c (make-object color% "white"))
      (define (->b x) (inexact->exact (round x)))
      (send dc set-background c)
      (send dc clear)
      (define (draw-sprite s)
        (match-define (sprite-data x y r g b a spr pal mx my theta) s)
        (send dc set-origin x y)
        (send dc set-scale mx my)
        (send dc set-rotation (* theta (/ 180.0 pi)))
        (define ->w*h*bs (hash-ref n->bm spr))
        (define-values (w h bs) (->w*h*bs))
        (for* ([bx (in-range w)]
               [by (in-range h)])
          (define-syntax-rule (define-nc nr i r)
            (begin (define cr (pixel-ref bs w h bx by i))
                   (define nr (->b (* cr r)))))
          (define-nc nr 1 r)
          (define-nc ng 2 g)
          (define-nc nb 3 b)
          (define-nc na 0 a)
          (send c set nr ng nb 1.0) ;; xxx using anything but 1.0 messes up colors
          (send dc set-pen c 1 'solid)
          (send dc draw-point bx by)))
      (tree-for draw-sprite sprite-tree))
    (paint root-bm-dc)
    (send root-bm save-file "lambda.png" 'png 100 #:unscaled? #t)
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
