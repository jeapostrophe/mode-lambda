#lang racket/base
(require racket/math
         mode-lambda)

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
  (define csd (compile-sprite-db sd))
  (define draw (make-draw csd W H))
  (define s
    (for/list ([i (in-range (* 2 W))])
      (define n (list-ref ns (random (length ns))))
      (sprite (* W (random)) (* H (random))
              (random) (random) (random) (+ 0.5 (* 0.5 (random)))
              n #f
              (* (random) 2) (* (random) 2)
              (* (random) 2 pi))))
  (time
   (for ([i (in-range 4)])
     (draw s))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "one"
   #:args (p)
   (go p)))
