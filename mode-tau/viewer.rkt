#lang racket/base
(require racket/runtime-path
         racket/file
         racket/string
         racket/flonum
         racket/fixnum
         mode-tau
         lux
         lux/chaos/gui
         lux/chaos/gui/key)

(define-runtime-path here ".")
(define me (build-path here "main.rkt"))

(struct viewer (out)
  #:methods gen:word
  [(define (word-fps w)
     0.0)
   (define (word-label s ft)
     (lux-standard-label "Mode-τ" ft))
   (define (word-output w)
     (viewer-out w))
   (define (word-event w e)
     (cond
       [(or (eq? e 'close)
            (and (key-event? e)
                 (eq? 'escape (key-event-code e))))
        #f]
       [else
        w]))])

(define (string->view-output s)
  (define gdb (make-glyph-db))
  (define the-font
    (load-font! gdb
                #:size 24.0
                #:face "Triplicate T4c"
                #:smoothing 'smoothed
                #:family 'modern))
  (define cgdb (compile-glyph-db gdb))

  (define a-char (font-glyph-idx the-font cgdb #\A))
  (define font-width (glyph-width cgdb a-char))
  (define font-height (glyph-height cgdb a-char))
  (define render (stage-render cgdb))
  (render
   (for/list ([line (in-list (string-split s "\n"))]
              [row (in-naturals)])
     (for/list ([char (in-string line)]
                [col (in-naturals)])
       (glyph (fl* (fl+ 0.5 (fx->fl col)) font-width)
              (fl* (fl+ 0.5 (fx->fl row)) font-height)
              (font-glyph-idx the-font cgdb char)
              #:fgr 255 #:fgg   0 #:fgb 0
              #:bgr   0 #:bgg 255 #:bgb 0)))))

(module+ main
  (call-with-chaos
   (make-gui #:mode gui-mode)
   (λ ()
     (fiat-lux (viewer (string->view-output (file->string me)))))))
