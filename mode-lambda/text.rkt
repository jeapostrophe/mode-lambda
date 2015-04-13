#lang racket/base
(require racket/match
         mode-lambda)

(struct *ml-font (char->char-id))
(define *ALL-ASCII*
  (for/fold ([l '()]) ([i (in-range 256)])
    (define c (integer->char i))
    (if (or (char=? #\space c) (char-graphic? c))
        (cons c l)
        l)))
(define (load-font!/font% sd f-id f%
                          #:alphabet [alphabet *ALL-ASCII*])
  (local-require (prefix-in pict: pict))
  (define char->char-id
    (for/hasheq ([c (in-list alphabet)])
      (define ci (char->integer c))
      (define char-id (string->symbol (format "font:~a:~v" f-id ci)))
      (define char-v (pict:text (string c) f%))
      (add-sprite!/value sd char-id char-v)
      (values c char-id)))
  (*ml-font char->char-id))

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

(provide load-font!/font%
         make-text-renderer)
