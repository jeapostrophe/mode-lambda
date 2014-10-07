#lang racket/base
(require racket/runtime-path
         racket/match
         racket/math
         file/untar
         file/gunzip
         gfx/color
         mode-lambda
         mode-lambda/backend/software)

(define (random-byte) (random 256))

(define-runtime-path here ".")

(define (go mode)
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
       (add-palette!/file sd 'block-pal (build-path p "blocks" "pal.png"))
       (define (add! i)
         (define n (string->symbol (format "Block~a" i)))
         (add-sprite!/file sd n (build-path p "blocks" (format "~a.png" n))
                           #:palette 'block-pal)
         n)
       (for/list ([i (in-range 3)]) (add! i)))
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
  (define (random-vector-ref l)
    (vector-ref l (random (vector-length l))))
  (define (random-list-ref l)
    (list-ref l (random (length l))))
  (define (random-spr)
    (random-list-ref ns))
  (define original-csd (compile-sprite-db sd))
  (define csd-p (build-path here "csd"))
  (save-csd! original-csd csd-p)
  (define csd (load-csd csd-p))
  (define (random-spr-idx)
    (sprite-idx csd (random-spr)))
  (define draw (make-draw csd W H))
  (define s
    ;; xxx make a layers mode to test layers
    (match mode
      ["rand"
       (for/list ([i (in-range (* 2 W))])
         (sprite 0 (* W (random)) (* H (random))
                 (random-byte) (random-byte) (random-byte) (+ 0.5 (* 0.5 (random)))
                 (random-spr-idx) 0
                 (* (random) 2) (* (random) 2)
                 (* (random) 2 pi)))]
      ["grid"
       (for*/list ([x (in-range W)]
                   [y (in-range W)])
         (sprite 0 (exact->inexact (* 16 x)) (exact->inexact (* 16 y))
                 0 0 0 1.0
                 (random-spr-idx) 0
                 1.0 1.0 0.0))]
      ["blocks"
       (define tetras (tetradic-idxs cw-slots))
       (match-define tetra (random-list-ref tetras))
       (for*/list ([x (in-range (sub1 (quotient W 8)))]
                   [y (in-range (sub1 (quotient H 8)))])
         (define block-style 0 #;(random 3))
         (define color-scheme (random-vector-ref tetra))
         (sprite 0 
                 (+ 3 4 (exact->inexact (* 8 x)))
                 (+ 3 4 (exact->inexact (* 8 y)))
                 0 0 0 1.0
                 (sprite-idx csd (string->symbol (format "Block~a" block-style)))
                 (palette-idx csd (string->symbol (format "hi~a" color-scheme)))
                 1.0 1.0 0.0))]))
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
  (require racket/cmdline)
  (command-line
   #:args (mode)
   (go mode)))
