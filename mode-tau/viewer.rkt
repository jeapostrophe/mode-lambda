#lang racket/base
(require racket/file
         racket/string
         racket/flonum
         racket/fixnum
         racket/match
         mode-tau
         lux
         lux/chaos/gui
         lux/chaos/gui/key)

(define gdb (make-glyph-db))
(define the-font
  (load-font! gdb
              #:size 13.0
              #:face "Triplicate T4c"
              #:family 'modern))
(define cgdb (compile-glyph-db gdb))

(define a-char (font-glyph-idx the-font cgdb #\A))
(define font-width (glyph-width cgdb a-char))
(define font-height (glyph-height cgdb a-char))
(define render (stage-render cgdb))

(struct viewer (w h s r)
  #:methods gen:word
  [(define (word-fps w)
     60.0)
   (define (word-label s ft)
     (lux-standard-label "Mode-τ" ft))
   (define (word-output v)
     (match-define (viewer w h s r) v)
     (define max-rows-per-screen
       (inexact->exact (floor (/ h font-height))))
     (define start-row
       (max 0 (min (- (vector-length s) max-rows-per-screen) r)))
     (render
      255 255 255
      (for/list ([srow (in-range max-rows-per-screen)])
        (define arow (+ srow start-row))
        (define line (vector-ref s arow))
        (for/list ([char (in-string line)]
                   [col (in-naturals)])
          (glyph (fl* (fl+ 0.5 (fx->fl col)) font-width)
                 (fl* (fl+ 0.5 (fx->fl srow)) font-height)
                 (font-glyph-idx the-font cgdb char)
                 #:fgr 0 #:fgg   0 #:fgb 0)))))
   (define (word-event v e)
     (match e
       [(or 'close
            (and (? key-event?)
                 (app key-event-code 'escape)))
        #f]
       [`(resize ,w ,h)
        (struct-copy viewer v
                     [w w] [h h])]
       [_
        v]))
   (define (word-tick v)
     (struct-copy viewer v
                  [r (random (vector-length (viewer-s v)))]))])

(module+ main
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (define me (build-path here "main.rkt"))

  (call-with-chaos
   (make-gui #:mode gui-mode)
   (λ ()
     (fiat-lux
      (viewer 800 600 (list->vector (string-split (file->string me) "\n"))
              0)))))
