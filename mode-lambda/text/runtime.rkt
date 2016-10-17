#lang racket/base
(require racket/match
         mode-lambda)

(struct *ml-font (char->char-id) #:prefab)
(module+ private
  (provide *ml-font))

(define (font? x)
  (*ml-font? x))

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
        (unless idx
          (local-require mode-lambda/core)
          (error 'make-text-renderer "Cannot find sprite ~v" ci))
        idx))
    (define-values (width height)
      (for/fold ([w 0] [h 0]) ([i (in-list idxs)])
        (values (+   w (* mx (sprite-width csd i)))
                (max h (* my (sprite-height csd i))))))
    (define sx (- tx (/ width 2.0)))
    (define  y (+ ty (/ height 2.0)))
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

(provide font?
         font-char-idx
         make-text-renderer)
