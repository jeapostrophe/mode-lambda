#lang racket/base
(require racket/match
         mode-lambda)

(struct *ml-font (char->char-id))

(define (font? x)
  (*ml-font? x))

(define *ALL-ASCII*
  (for/fold ([l '()]) ([i (in-range 256)])
    (define c (integer->char i))
    (if (or (char=? #\space c) (char-graphic? c))
        (cons c l)
        l)))
(define (load-font! sd
                    #:size [size 12]
                    #:face [face #f]
                    #:family [family 'default]
                    #:style [style 'normal]
                    #:weight [weight 'normal]
                    #:underlined? [underlined? #f]
                    #:smoothing [smoothing 'default]
                    #:size-in-pixels? [size-in-pixels? #f]
                    #:hinting [hinting 'aligned]
                    #:alphabet [alphabet *ALL-ASCII*])
  (local-require racket/class
                 racket/gui/base)
  
  (define f%
    (make-font #:size size #:face face #:family family
               #:style style #:weight weight #:underlined? underlined?
               #:smoothing smoothing #:size-in-pixels? size-in-pixels?
               #:hinting hinting))
  (define f-id
    (string->symbol
     (format "~v"
             `(make-font ,size ,face ,family
                         ,style ,weight ,underlined?
                         ,smoothing ,size-in-pixels?
                         ,hinting))))

  (define size-dc (send (make-screen-bitmap 1 1) make-dc))
  (send size-dc set-font f%)
  (define-values (width.0 height.0 xtra-below xtra-above)
    (send size-dc get-text-extent " "))
  (define width (inexact->exact (ceiling width.0)))
  (define height (inexact->exact (ceiling height.0)))
  
  (define char->char-id
    (for/hasheq ([c (in-list alphabet)])
      (define ci (char->integer c))
      (define char-id (string->symbol (format "font:~a:~v" f-id ci)))
      (define make-char-bm
        (λ ()
          (define char-bm (make-platform-bitmap width height
                                                #:backing-scale 8))
          (define char-dc (send char-bm make-dc))

          (send char-dc set-font f%)
          (send char-dc draw-text (string c) 0 0)
          
          char-bm))
      
      (add-sprite!/bm sd char-id make-char-bm)
      (values c char-id)))
  (*ml-font char->char-id))

(define (font-char-idx f csd c)
  (match-define (*ml-font char->char-id) f)
  (define ci (hash-ref char->char-id c #f))
  (or (and ci (sprite-idx csd ci))
      0))

;; MAYBE this could be a lot more complicated, with colors and stuff
;; like that.
(define (make-text-renderer f csd)
  (match-define (*ml-font char->char-id) f)
  (λ (text tx ty
           #:layer [layer 0]
           #:mx [mx 1.0]
           #:my [my 1.0]
           #:r [r 0]
           #:g [g 0]
           #:b [b 0]
           #:a [a 1.0])
    (define idxs
      (for/list ([c (in-string text)])
        (define ci (hash-ref char->char-id c))
        (define idx (sprite-idx csd ci))
        idx))
    (define-values (width height)
      (for/fold ([w 0] [h 0]) ([i (in-list idxs)])
        (values (+ w (* mx (sprite-width csd i)))
                (max h (* my (sprite-height csd i))))))
    (define sx (- tx (/ width 2.0)))
    (define y (+ ty (/ height 2.0)))
    (define-values (lx st)
      (for/fold ([sx sx] [st #f])
                ([i (in-list idxs)])
        (define w (* mx (sprite-width csd i)))
        (define x (+ sx (/ w 2.0)))
        (values (+ sx w)
                (cons (sprite x y i
                              #:layer layer
                              #:mx mx #:my my
                              #:r r #:g g #:b b #:a a)
                      st))))
    st))

(provide *ALL-ASCII*
         font?
         load-font!
         font-char-idx
         make-text-renderer)
