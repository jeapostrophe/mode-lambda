#lang racket/base
(require racket/math
         gfx/color
         mode-lambda
         mode-lambda/backend/software)

(define (random-byte) (random 256))

(define (go p)
  (define W 256)
  (define H 224)
  (define sd (make-sprite-db))
  (define CW (color-wheel (* 1 2 3 4 2)))
  (define ps
    (for/list ([c (in-list CW)]
               [i (in-naturals)])
      (define n (string->symbol (format "cw~a" i)))
      (add-palette! sd n (color->palette c))
      n))
  (define sprs (build-path p "monochrome"))
  (define ns
    (append
     (let ()
       (define (add! n v)
         (sprite-db-add!/convert sd n v)
         n)
       (append
        (let ()
          (local-require 2htdp/image)
          (list (add! 'star (star 100 "solid" "black"))))
        (let ()
          (local-require pict)
          (list (add! 'fish (standard-fish 100 50))))))
     (for/list ([f (in-list (directory-list sprs))])
       (define n (string->symbol (regexp-replace #rx".png$" (path->string f) "")))
       (sprite-db-add!/file sd n (build-path sprs f))
       n)))
  (define (random-spr)
    (list-ref ns (random (length ns))))
  (define original-csd (compile-sprite-db sd))
  (save-csd! original-csd "csd")
  (define csd (load-csd "csd"))
  (define (random-spr-idx)
    (sprite-idx csd (random-spr)))
  (define draw (make-draw csd W H))
  (define s
    (if #t
        (for/list ([i (in-range (* 2 W))])
          (sprite 0 (* W (random)) (* H (random))
                  (random-byte) (random-byte) (random-byte) (+ 0.5 (* 0.5 (random)))
                  (random-spr-idx) 0
                  (* (random) 2) (* (random) 2)
                  (* (random) 2 pi)))
        (for*/list ([x (in-range W)]
                    [y (in-range W)])
          (define i (min (sub1 (length ns))
                         (+ (* x (quotient (length ns) W)) y)))
          (define n (list-ref ns i))
          (sprite 0 (exact->inexact (* 16 x)) (exact->inexact (* 16 y))
                  0 0 0 1.0
                  (sprite-idx csd n) 0
                  1.0 1.0 0.0))))
  (define last-bs
    (time
     (for/fold ([bs #f]) ([i (in-range 4)])
       (draw s))))
  (let ()
    (local-require racket/draw
                   racket/class)
    (define root-bm
      (make-bitmap W H))
    (send root-bm set-argb-pixels 0 0 W H last-bs)
    (send root-bm save-file "lambda.png" 'png 100 #:unscaled? #t)))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "one"
   #:args (p)
   (go p)))
