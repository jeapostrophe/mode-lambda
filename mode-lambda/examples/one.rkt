#lang racket/base
(require racket/runtime-path
         racket/match
         racket/flonum
         racket/fixnum
         racket/math
         racket/file
         racket/list
         racket/string
         file/untar
         file/gunzip
         gfx/color
         mode-lambda
         mode-lambda/backend/software)

(define (random-byte) (random 256))

(define-runtime-path here ".")

(define (parse-blocks p)
  (define block-data (rest (string-split (file->string p) "\n\n")))
  (for/vector ([bd (in-list block-data)])
    (define lines (string-split bd "\n"))
    (define R (build-vector 4 (λ (i) (build-vector 4 (λ (i) (make-bytes 4 0))))))
    (for ([ld (in-list lines)]
          [y (in-naturals)])
      (for ([rd (in-list (string-split ld))]
            [r (in-naturals)])
        (for ([xd (in-string rd)]
              [x (in-naturals)])
          (bytes-set! (vector-ref (vector-ref R r) y) x
                      (if (char=? #\0 xd)
                          0
                          1)))))
    R))

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

  ;; SNES
  (define W 256)
  (define H 224)
  ;; GB-SNES
  (set! W (* 26 16))
  (set! H (* 26 9))
  (define sd (make-sprite-db))
  (define (add-cw! CW fmt)
    (for/list ([c (in-list CW)]
               [i (in-naturals)])
      (define n (string->symbol (format fmt i)))
      (add-palette! sd n (color->palette c))
      n))
  (define cw-slots (* 3 4 7))
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
  (define render (stage-render csd W H))
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
       (define schemes (polygon-idxs 7 cw-slots))
       (match-define scheme (random-list-ref schemes))
       (define blocks (parse-blocks (build-path here "blocks.txt")))
       (define block-styles
         (for/vector ([b (in-vector blocks)])
           (sprite-idx csd (string->symbol (format "Block~a" (random 3))))))
       (define color-schemes
         (for/vector ([c (in-vector scheme)])
           (palette-idx csd (string->symbol (format "hi~a" c)))))
       (define block-sprites
         (for*/list ([c (in-range (quotient (quotient W 8) 4))]
                     [r (in-range (quotient (quotient H 8) 4))])
           (define block (random 7))
           (define block-data (vector-ref blocks block))
           (define rotation (random-vector-ref block-data))
           (for*/list ([cc (in-range 4)]
                       [rr (in-range 4)])
             (when (= 1 (bytes-ref (vector-ref rotation rr) cc))
               (define x (fx+ (fx* 4 c) cc))
               (define y (fx+ (fx* 4 r) rr))
               (sprite 4
                       (+ 3 4 0.5 (fx->fl (fx* 8 x)))
                       (+ 3 4 0.5 (fx->fl (fx* 8 y)))
                       0 0 0 1.0
                       (vector-ref block-styles block)
                       (vector-ref color-schemes block)
                       1.0 1.0 0.0)))))
       (define background-sprites
         (for*/list ([x (in-range (quotient W 8))]
                     [y (in-range (quotient H 8))])
           (when (or (and (even? y) (even? x))
                     (and (odd? x) (odd? y)))
             (sprite 0
                     (fl+ 4.0 (fx->fl (fx* 8 x)))
                     (fl+ 4.0 (fx->fl (fx* 8 y)))
                     0 0 0 1.0
                     (vector-ref block-styles 0)
                     (palette-idx csd 'grayscale)
                     1.0 1.0 0.0))))
       (define foreground-sprites
         (for*/list ([x (in-range (quotient W 8))]
                     [y (in-range (quotient H 8))])
           (when (or (and (even? x) (odd? y))
                     (and (even? y) (odd? x)))
             (sprite 7
                     (fl+ 4.0 (fx->fl (fx* 8 x)))
                     (fl+ 4.0 (fx->fl (fx* 8 y)))
                     0 0 0 0.5
                     (vector-ref block-styles 1)
                     (palette-idx csd 'med0)
                     1.0 1.0 0.0))))
       (list* block-sprites background-sprites foreground-sprites)]))
  (define last-bs
    (time
     (for/fold ([bs #f]) ([i (in-range 4)])
       (render (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)) 1.0 1.0 0.0)
                       #f #f #f
                       (layer (fx->fl (/ W 2)) (fl+ (fx->fl (/ H 2)) 25.0)
                              1.0 1.0
                              (fl/ pi 4.0))
                       #;(layer (fx->fl (/ W 2)) (fx->fl (/ H 2))
                              1.0 1.0 0.0)
                       #f #f
                       (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)) 2.0 2.0 0.0))
               s))))
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
