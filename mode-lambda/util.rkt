#lang racket/base
(require racket/match
         racket/fixnum
         racket/contract/base
         (except-in ffi/unsafe ->))

(define-syntax-rule (define-cstruct&list struct-id info-id ([field _type] ...))
  (begin (define-cstruct struct-id ([field _type] ...))
         (define info-id (list (cons 'field _type) ...))))

(define (tree-for f t)
  (match t
    [(or #f (? void?) '()) (void)]
    [(cons a d)
     (tree-for f a)
     (tree-for f d)]
    [_
     (f t)]))

(define (tree-fold f ret t)
  (match t
    [(or #f (? void?) '()) ret]
    [(cons a d)
     (tree-fold f (tree-fold f ret a) d)]
    [_
     (f ret t)]))

(define (count-objects t)
  (tree-fold (λ (count o) (fx+ 1 count)) 0 t))

(define tree/c
  any/c)

(provide (all-defined-out))
