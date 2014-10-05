#lang racket/base
(require racket/math
         mode-lambda)

(define (random-byte) (random 256))

(define (go p)
  (define W 256)
  (define H 224)
  (define sd (make-sprite-db))
  (define sprs (build-path p "sprs"))
  (define ns
    (for/list ([f (in-list (directory-list sprs))])
      (define n (string->symbol (regexp-replace #rx".png$" (path->string f) "")))
      (sprite-db-add!/file sd n (build-path sprs f))
      n))
  (define (random-spr)
    (list-ref ns (random (length ns))))
  (define csd (compile-sprite-db sd))
  (define (random-spr-idx)
    (sprite-idx csd (random-spr)))
  (save-csd! csd "csd")
  (define draw (make-draw csd W H))
  (define s
    (if #t
        (for/list ([i (in-range (* 2 W))])
          (sprite (* W (random)) (* H (random))
                  (random-byte) (random-byte) (random-byte) (random-byte)
                  (random-spr-idx) 0
                  (* (random) 2) (* (random) 2)
                  (* (random) 2 pi)))
        (for*/list ([x (in-range W)]
                    [y (in-range W)])
          (sprite (exact->inexact (* 16 x)) (exact->inexact (* 16 y))
                  0 0 0 255
                  (random-spr-idx) 0
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
