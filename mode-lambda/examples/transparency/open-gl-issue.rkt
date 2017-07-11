#lang racket/base
(require racket/file
         racket/runtime-path
         lux
         lux/chaos/gui
         lux/chaos/gui/key
         mode-lambda
         mode-lambda/static
         mode-lambda/shot
         (prefix-in soft: mode-lambda/backend/software)
         (prefix-in gl: mode-lambda/backend/gl))

(define-runtime-path here ".")

(define (go! gl?)
  (define W 640)
  (define H 480)
  (define hw.0 (/ W 2.0))
  (define hh.0 (/ H 2.0))
  
  (define stage-draw/dc
    (if gl?
      gl:stage-draw/dc
      soft:stage-draw/dc))

  (define cdir (build-path here "gl-shot"))
  (make-directory* cdir)
  (gl:gl-screenshot!
   (screenshot-in-dir! cdir))

  (define sprite-db              (make-sprite-db))
  (add-sprite!/file sprite-db    'game-over (build-path here "game-over.png"))
  (define compiled-db            (compile-sprite-db sprite-db))
  (save-csd! compiled-db (build-path here "tmp") #:debug? #t)
  (define game-over-index        (sprite-idx compiled-db 'game-over))

  (define test-layer             (layer hw.0 hh.0))
  (define layer-config           (vector test-layer))

  (define rendering-states->draw
    (stage-draw/dc compiled-db W H (vector-length layer-config)))

  (struct demo ()
    #:methods gen:word

    [(define (word-output w)
       (define game-over-sprite  (sprite hw.0 hh.0 game-over-index #:layer 0))
       (define dynamic-sprites   (list game-over-sprite))

       (rendering-states->draw layer-config '() dynamic-sprites))

     (define (word-event w e)
       (cond
         [(or (eq? e 'close)
              (and (key-event? e)
                   (or (eq? 'escape (key-event-code e))
                       (eq? #\q (key-event-code e)))))
          #f]         
         [else w]))

     (define (word-tick w) w)])

  (call-with-chaos
   (make-gui #:mode (if gl? 'gl-core 'draw) #:width W #:height H)
   (λ () (fiat-lux (demo)))))

(module+ main
  (require racket/match
           racket/cmdline)
  (command-line #:program "open-gl-issue"
                #:args (vers)
                (go! (match vers
                       ["o" #t]
                       ["s" #f]))))
