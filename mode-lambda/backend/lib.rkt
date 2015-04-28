#lang racket/base
(require mode-lambda/core
         racket/draw
         racket/fixnum
         racket/flonum
         racket/contract/base
         racket/class)

(define (argb-bytes->bitmap w h bs)
  (define root-bm (make-bitmap w h))
  (send root-bm set-argb-pixels 0 0 w h bs)
  root-bm)

(define (save-bitmap! bm p)
  (send bm save-file p 'png 100 #:unscaled? #t)
  (void))

;; We want to find how to scale the CRT to the real screen, but it is
;; important to only use powers of two in the decimals and only up to
;; 2^5
(define (quotient* x y)
  (define-values (q r) (quotient/remainder x y))
  (define (recur r i max-i)
    (cond
      [(= i max-i)
       0]
      [else
       (define d (expt 2 (* -1 i)))
       (define dy (* d y))
       (cond
         [(> dy r)
          (recur r (add1 i) max-i)]
         [else
          (+ d (recur (- r dy) (add1 i) max-i))])]))
  (+ q (recur r 1 5)))

(define (compute-nice-scale w W h H)
  (fx->fl
   (fxmin (fxquotient w W)
          (fxquotient h H))))

(define (draw-bitmap! w W h H bm dc)
  (send dc set-background "black")
  (send dc clear)
  (define scale (compute-nice-scale w W h H))
  (define SW (* scale W))
  (define SH (* scale H))
  (define x (/ (/ (- w SW) 2) scale))
  (define y (/ (/ (- h SH) 2) scale))
  (send dc set-scale scale scale)
  (send dc draw-bitmap bm x y)
  (void))

(define draw/dc/c
  (backend/c ()
             (-> exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 any/c
                 any)))

(provide
 (contract-out
  [argb-bytes->bitmap
   (-> exact-nonnegative-integer?
       exact-nonnegative-integer?
       bytes?
       (is-a?/c bitmap%))]
  [save-bitmap!
   (-> (is-a?/c bitmap%)
       path-string?
       void?)]
  [draw-bitmap!
   (-> exact-nonnegative-integer? exact-nonnegative-integer?
       exact-nonnegative-integer? exact-nonnegative-integer?
       (is-a?/c bitmap%)
       (is-a?/c dc<%>)
       void?)]
  [compute-nice-scale
   (-> exact-nonnegative-integer? exact-nonnegative-integer?
       exact-nonnegative-integer? exact-nonnegative-integer?
       real?)]
  [draw/dc/c
   contract?]))
