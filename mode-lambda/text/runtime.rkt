#lang racket/base
(require racket/match
         racket/fixnum
         racket/flonum
         mode-lambda)

(struct *ml-font (scaling char->char-id) #:prefab)
(module+ private
  (provide *ml-font))

(define font? *ml-font?)

(define (font-char-idx f csd c)
  (match-define (*ml-font _ char->char-id) f)
  (define ci (hash-ref char->char-id c #f))
  (or (and ci (sprite-idx csd ci))
      0))

;; MAYBE this could be a lot more complicated, with colors and stuff
;; like that.
(define (make-text-renderer f csd)
  (match-define (*ml-font scaling char->char-id) f)
  (λ (text tx ty
           #:layer [layer 0]
           #:mx [mx^ 1.0]
           #:my [my^ 1.0]
           #:r [r 0]
           #:g [g 0]
           #:b [b 0]
           #:a [a 1.0])
    (define 1/scaling (fl/ 1.0 scaling))
    (define mx (fl* 1/scaling mx^))
    (define my (fl* 1/scaling my^))
    (define idxs
      (for/list ([c (in-string text)])
        (define ci (hash-ref char->char-id c))
        (define idx (sprite-idx csd ci))
        (unless idx
          (local-require mode-lambda/core)
          (error 'make-text-renderer "Cannot find sprite ~v" ci))
        idx))
    (define-values (width height)
      (for/fold ([w 0.0] [h 0.0]) ([i (in-list idxs)])
        (values (fl+   w (fl* mx (fx->fl (sprite-width csd i))))
                (flmax h (fl* my (fx->fl (sprite-height csd i)))))))
    (define sx (fl- tx (fl/ width 2.0)))
    (define  y (fl+ ty (fl/ height 2.0)))
    (define-values (lx st)
      (for/fold ([sx sx] [st #f])
                ([i (in-list idxs)])
        (define w (fl* mx (fx->fl (sprite-width csd i))))
        (define x (fl+ sx (fl/ w 2.0)))
        (values (fl+ sx w)
                (cons (sprite x y i
                              #:layer layer
                              #:mx mx #:my my
                              #:r r #:g g #:b b #:a a)
                      st))))
    st))

(provide font?
         font-char-idx
         make-text-renderer)
