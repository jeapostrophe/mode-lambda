#lang racket/base
(require pict
         pict/flash
         mode-lambda
         mode-lambda/static)

;;;
;;; SIZES
;;;

(define W 400)
(define H 400)
(define W/2 (/ W 2.))
(define H/2 (/ H 2.))

;;;
;;; BITMAPS
;;;

(define gray-p
  (colorize (filled-rectangle W H) "gray"))
(define fish-p
  (standard-fish  100 50))
(define lantern-p
  (jack-o-lantern 100))
(define flash-p
  (colorize (linewidth 5 (outline-flash 100 105)) "yellow"))

;;;
;;; SPRITES
;;;

(define db (make-sprite-db))

(add-sprite!/value db 'gray    gray-p)
(add-sprite!/value db 'fish    fish-p)
(add-sprite!/value db 'lantern lantern-p)
(add-sprite!/value db 'flash   flash-p)

(define cdb (compile-sprite-db db))

(module+ test
  (require racket/runtime-path)
  (define-runtime-path jens "jens")
  (save-csd! cdb jens #:debug? #t))

(define gray-idx    (sprite-idx cdb 'gray))
(define fish-idx    (sprite-idx cdb 'fish))
(define lantern-idx (sprite-idx cdb 'lantern))
(define flash-idx   (sprite-idx cdb 'flash))

;;;
;;; SPRITES
;;;

(define gray-sprite    (sprite W/2  H/2  gray-idx    #:layer 0)) ; bug layer
(define fish-sprite    (sprite 200. 200. fish-idx    #:layer 1)) ; background
(define lantern-sprite (sprite 250. 200. lantern-idx #:layer 2)) ; middle
(define flash-sprite   (sprite 160. 200. flash-idx   #:layer 3)) ; foreground

;;;
;;; RUNTIME
;;;

(define static (list gray-sprite fish-sprite lantern-sprite))
(define dynamic (list flash-sprite))

(module+ main
  (require "quick.rkt")
  (quick-run cdb static dynamic
             #:width W #:height H))
