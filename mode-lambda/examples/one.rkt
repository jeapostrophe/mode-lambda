#lang racket/base
(require racket/runtime-path
         racket/math
         file/untar
         file/gunzip
         gfx/color
         mode-lambda
         mode-lambda/backend/software)

(define (random-byte) (random 256))

(define-runtime-path here ".")

(define (go)
  (define p (build-path here "edb"))
  (define sprs (build-path p "monochrome"))
  (unless (directory-exists? sprs)
    (define-values (in-bytes out-bytes) (make-pipe))
    (call-with-input-file (build-path p "monochrome.tgz")
      (λ (in-file)
        (gunzip-through-ports in-file out-bytes)))
    (close-output-port out-bytes)
    (untar in-bytes
           #:dest p))

  (define W 256)
  (define H 224)
  (define sd (make-sprite-db))
  (define (add-cw! CW fmt)
    (for/list ([c (in-list CW)]
               [i (in-naturals)])
      (define n (string->symbol (format fmt i)))
      (add-palette! sd n (color->palette c))
      n))
  (define cw-slots (* 1 2 3 4 2))
  (define ps
    (append (list (add-palette! sd 'grayscale (color->palette GRAY))
                  'grayscale)
            (add-cw! (color-wheel cw-slots) "hi~a")
            (add-cw! (color-wheel cw-slots #:s 0.67 #:b 0.6) "med~a")))

  (define ns
    (append
     (let ()
       (define (add! n v)
         (add-sprite!/value sd n v)
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
       (add-sprite!/file sd n (build-path sprs f))
       n)))
  (define (random-spr)
    (list-ref ns (random (length ns))))
  (define original-csd (compile-sprite-db sd))
  (define csd-p (build-path here "csd"))
  (save-csd! original-csd csd-p)
  (define csd (load-csd csd-p))
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
    (send root-bm save-file (build-path here "lambda.png") 'png 100 #:unscaled? #t)))

(module+ main
  (go))
