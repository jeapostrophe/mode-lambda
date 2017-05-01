#lang racket/base
(require lux
         lux/chaos/gui
         mode-lambda
         mode-lambda/static
         mode-lambda/shot
         (prefix-in soft: mode-lambda/backend/software)
         (prefix-in gl: mode-lambda/backend/gl))

(define gl? #t)

(define stage-draw/dc
  (if gl?
    gl:stage-draw/dc
    soft:stage-draw/dc))

(gl:gl-screenshot!
 (screenshot-in-dir! "gl-shot"))

(define sprite-db              (make-sprite-db))
(add-sprite!/file sprite-db    'game-over "game-over.png")
(define compiled-db            (compile-sprite-db sprite-db))
(save-csd! compiled-db "tmp" #:debug? #t)
(define game-over-index        (sprite-idx compiled-db 'game-over))

(define test-layer             (layer 320.0 240.0))
(define layer-config           (vector test-layer))

(define rendering-states->draw
  (stage-draw/dc compiled-db 640 480 (vector-length layer-config)))

(struct demo ()
  #:methods gen:word

  [(define (word-output w)
     (define game-over-sprite  (sprite 320.0 240.0 game-over-index #:layer 0))
     (define dynamic-sprites   (list game-over-sprite))

     (rendering-states->draw layer-config '() dynamic-sprites))

   (define (word-event w e)
     (cond
       [(eq? e 'close) #f]
       [else w]))

   (define (word-tick w) w)])

; main
(module+ main
  (call-with-chaos
   (make-gui #:mode (if gl? 'gl-core 'draw) #:width 640 #:height 480)
   (λ () (fiat-lux (demo)))))

