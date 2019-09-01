#lang racket/base
(require mode-lambda
         mode-lambda/static
         mode-lambda/text)

(define W 800) (define H 600)

;; look at alacrity --- https://github.com/jwilm/alacritty/blob/b0efa9d105b53211d8df094238c7eb8324e93566/font/src/lib.rs#L17

;; xxx test sequences of l
;; xxx test weird dippy doo fonts like papyrus

(define db (make-sprite-db))
(define f:trip (load-font! db #:face "Triplicate T4c"))
(define f:roman (load-font! db #:face #;"Consolas" "Times New Roman"))

(define cdb (compile-sprite-db db))
(define t:trip (make-text-renderer f:trip cdb))
(define t:roman (make-text-renderer f:roman cdb))

(define test
  (string-append "** TODO Improve mode-lambda text rendering :Work:"
                 "** TODO Improve mode-lambda text rendering :Work:"))
(define static '())
(define dynamic
  (list
   (t:trip test
           (* W 0.5) (* H 0.25)
           #:r 255 #:g 255 #:b 255)
   (t:roman test
            (* W 0.5) (* H 0.75)
           #:r 255 #:g 255 #:b 255)))

(module+ main
  (require "quick.rkt")
  (quick-run cdb static dynamic
             #:width W #:height H))
