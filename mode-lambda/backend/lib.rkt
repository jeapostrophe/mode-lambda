#lang racket/base
(require mode-lambda/core
         racket/fixnum
         racket/flonum
         racket/contract/base)

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

(define (compute-nice-scale par w W h H)
  (define chunky-scale
    (fx->fl
     (fxmin (fxquotient w (fl->fx (flceiling (fl* par (fx->fl W)))))
            (fxquotient h H))))
  (if (fl= 0.0 chunky-scale)
      (flmin (fl/ (fx->fl w) (fx->fl W))
             (fl/ (fx->fl h) (fx->fl H)))
      chunky-scale))

(module+ test
  (compute-nice-scale 1.0 800 2560 578 1688))

(module+ test
  (define CRT-W 640)
  (define CRT-H 480)
  (define NES-W 256)
  (define NES-H 240)
  (define pixel-aspect-ratio (fl/ 8.0 7.0))
  
  (define the-scale
    (compute-nice-scale pixel-aspect-ratio
                        CRT-W NES-W
                        CRT-H NES-H))
  (list 'the-scale the-scale)
  (list 'y-scale the-scale)
  (list 'x-scale (* pixel-aspect-ratio the-scale)))

(define draw/dc/c
  (backend/c ()
             (-> exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 any/c
                 any)))

(provide
 (contract-out
  [compute-nice-scale
   (-> real?
       exact-nonnegative-integer? exact-nonnegative-integer?
       exact-nonnegative-integer? exact-nonnegative-integer?
       real?)]
  [draw/dc/c
   contract?]))
