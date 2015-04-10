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
         lux
         lux/chaos/gui
         lux/chaos/gui/key)

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

;; SNES
(define W 256)
(define H 224)
;; GB-SNES
(define GB-SNES-SCALE 26)
(set! W (* GB-SNES-SCALE 16))
(set! H (* GB-SNES-SCALE 9))
(define cw-slots (* 3 7))

(define (prepare-renderi stage-draw/dc)
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

  (define sd (make-sprite-db))
  (define (add-cw! CW fmt)
    (for/list ([c (in-list CW)]
               [i (in-naturals)])
      (define n (string->symbol (format fmt i)))
      (add-palette! sd n (color->palette c))
      n))
  (define ps
    (append (list (add-palette! sd 'grayscale (color->palette GRAY))
                  'grayscale)
            (add-cw! (color-wheel cw-slots) "hi~a")
            (add-cw! (color-wheel cw-slots #:s 0.67 #:b 0.6) "med~a")))
  (define ns
    (append
     (let ()
       ;; xxx the block borders look wrong
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

  (define original-csd (compile-sprite-db sd))
  (define csd-p (build-path here "csd"))
  (save-csd! original-csd csd-p)
  (define csd (load-csd csd-p))
  (define render (stage-draw/dc csd W H))
  (vector ns csd render))

(define (go renderi mode)
  (match-define (vector ns csd render) renderi)
  (define (random-vector-ref l)
    (vector-ref l (random (vector-length l))))
  (define (random-list-ref l)
    (list-ref l (random (length l))))
  (define (random-spr)
    (random-list-ref ns))
  (define (random-spr-idx)
    (sprite-idx csd (random-spr)))
  (define-values
    (s dt lct)
    (match mode
      ["rand"
       (values
        '()
        (λ ()
          (for/list ([i (in-range (* 2 W))])
            (sprite (* W (random)) (* H (random))
                    #:r (random-byte) #:g (random-byte) #:b (random-byte)
                    #:a (+ 0.5 (* 0.5 (random)))
                    (random-spr-idx)
                    #:mx (* (random) 2) #:my (* (random) 2)
                    #:theta (* (random) 2 pi))))
        (λ ()
          (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)))
                  #f #f #f #f #f #f #f)))]
      ["grid"
       (values
        '()
        (λ ()
          (for*/list ([x (in-range (quotient W 10))]
                      [y (in-range (quotient W 10))])
            (sprite (exact->inexact (* 16 x)) (exact->inexact (* 16 y))
                    (random-spr-idx))))
        (λ ()
          (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)))
                  #f #f #f #f #f #f #f)))]
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
       ;;(printf "color-schemes: ~v\n" color-schemes)
       (define background-sprites
         (for*/list ([x (in-range (quotient W 8))]
                     [y (in-range (quotient H 8))])
           (when (or (and (even? y) (even? x))
                     (and (odd? x) (odd? y)))
             (sprite (fl+ 4.0 (fx->fl (fx* 8 x)))
                     (fl+ 4.0 (fx->fl (fx* 8 y)))
                     (vector-ref block-styles 0)
                     #:pal-idx (palette-idx csd 'grayscale)))))
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
               (sprite #:layer 4
                       (+ 3 4 0.5 (fx->fl (fx* 8 x)))
                       (+ 3 4 0.5 (fx->fl (fx* 8 y)))
                       (vector-ref block-styles block)
                       #:pal-idx (vector-ref color-schemes block))))))

       ;; xxx these are very light in the GL version
       (define foreground-sprites
         (for*/list ([x (in-range (quotient W 8))]
                     [y (in-range (quotient H 8))])
           (when (or (and (even? x) (odd? y))
                     (and (even? y) (odd? x)))
             (sprite (fl+ 4.0 (fx->fl (fx* 8 x)))
                     (fl+ 4.0 (fx->fl (fx* 8 y)))
                     (vector-ref block-styles 1)
                     #:a 0.25
                     #:layer 7
                     #:pal-idx (palette-idx csd 'med0)))))

       (values (list* block-sprites foreground-sprites background-sprites)
               (λ ()
                 '())
               (λ ()
                 (vector
                  ;; xxx experiment with "driving" the background
                  (layer (fx->fl (/ W 2)) (fx->fl (/ H 2))
                         #:mode7 2.0
                         #:horizon 0.0
                         #:fov (fl* 8.0 (fl/ (fx->fl W) (fx->fl H))))
                  #f #f #f
                  (layer (fx->fl (/ W 2)) (fl+ (fx->fl (/ H 2)) 25.0)
                         #:theta (fl* (fl/
                                       (fx->fl
                                        (fxmodulo (fxquotient
                                                   (current-milliseconds)
                                                   10)
                                                  360))
                                       360.0)
                                      (fl* 2.0 pi)))
                  #f #f
                  (layer (fx->fl (/ W 2)) (fx->fl (/ H 2))
                         #:mx 2.0 #:my 2.0))))]
      ["tile"
       (values
        '()
        (λ ()
          (sprite (fx->fl (/ W 2)) (fx->fl (/ H 2))
                  (sprite-idx csd 'star)
                  #:r 255 #:g 255 #:b 255))
        (λ ()
          (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2))
                         #:hw (fl/ (fx->fl W) 8.0) #:hh (fl/ (fx->fl H) 8.0)
                         #:mx 0.5 #:my 0.5)
                  #f #f #f #f #f #f #f)))]
      ["wrapping"
       (define (star@ x y r g b)
         (sprite x y (sprite-idx csd 'star)
                 #:r r #:g g #:b b
                 #:mx 0.5 #:my 0.5))
       (values
        '()
        (λ ()
          (list (star@ 0.0 0.0 255 255 255)
                (star@ (fl- (fl* 1.0 (fl/ (fx->fl W) 4.0)) (fl* 4.0 (fx->fl W)))
                       (fl* 1.0 (fl/ (fx->fl H) 4.0))
                       128 0 255)
                (star@ (fl* 1.0 (fl/ (fx->fl W) 2.0))
                       (fl* 1.0 (fl/ (fx->fl H) 2.0))
                       255 0 255)
                (star@ (fl+ (fl* 3.0 (fl/ (fx->fl W) 4.0)) (fl* 4.0 (fx->fl W)))
                       (fl* 3.0 (fl/ (fx->fl H) 4.0))
                       255 0 128)
                (star@ 0.0 (fl* (fl/ 1.0 4.0) (fx->fl H)) 255 0 0)
                (star@ (fx->fl W) (fl* (fl/ 3.0 4.0) (fx->fl H)) 0 255 0)
                (star@ (fl* (fl/ 1.0 4.0) (fx->fl W)) 0.0 255 255 0)
                (star@ (fl* (fl/ 3.0 4.0) (fx->fl W)) (fx->fl H) 0 0 255)))
        (λ ()
          (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2))
                         #:hw (fx->fl (/ W 2)) #:hh (fx->fl (/ H 2))
                         #:wrap-x? #t #:wrap-y? #t
                         #:mx 0.5 #:my 0.5)
                  #f #f #f #f #f #f #f)))]))
  (λ ()
    (render (lct) s (dt))))

(struct one
  (renderi mode rt)
  #:methods gen:word
  [(define (word-fps w)
     60.0)
   (define (word-label s ft)
     (lux-standard-label "Mode-λ" ft))
   (define (word-output w)
     ((one-rt w)))
   (define (word-event w e)
     (define closed? #f)
     (cond
      [(or (eq? e 'close)
           (and (key-event? e)
                (eq? 'escape (key-event-code e))))
       #f]
      [(key-event? e)
       (define old (one-mode w))
       (define new
         (match (key-event-code e)
           [#\r "rand"]
           [#\g "grid"]
           [#\b "blocks"]
           [#\t "tile"]
           [#\w "wrapping"]
           [_ old]))
       (if (equal? old new)
           w
           (update-rt
            (struct-copy one w
                         [mode new])))]
      [else
       w]))
   (define (word-tick w)
     w)])


(define (update-rt w)
  (struct-copy one w
               [rt (go (one-renderi w)
                       (one-mode w))]))

(module+ main
  (require racket/cmdline
           (prefix-in gl: mode-lambda/backend/gl)
           (prefix-in soft: mode-lambda/backend/software))
  
  (define CONFIGS
    (hash "gl" (vector gl:gui-mode gl:stage-draw/dc)
          "soft" (vector soft:gui-mode soft:stage-draw/dc)))
  (define the-mode 'std)
  (define the-config "gl")

  (command-line
   #:once-any
   ["--std" "Use std gl filter"
    (set! the-mode 'std)]
   ["--crt" "Use crt gl filter"
    (set! the-mode 'crt)]
   #:once-any
   ["--gl" "Use gl version"
    (set! the-config "gl")]
   ["--soft" "Use soft version"
    (set! the-config "soft")])

  (current-pseudo-random-generator
   (vector->pseudo-random-generator
    (vector 0 0 1 0 0 1)))
  
  (gl:gl-filter-mode the-mode)

  (match-define (vector gui-mode stage-draw/dc) (hash-ref CONFIGS the-config))
  (call-with-chaos
   (make-gui #:mode gui-mode)
   (λ ()
     (fiat-lux (update-rt (one (prepare-renderi stage-draw/dc)
                               "blocks"
                               #f))))))
