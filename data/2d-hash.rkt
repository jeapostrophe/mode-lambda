#lang racket/base
(require racket/math
         racket/contract/base)

(struct 2d-hash (xbs xdiv ybs ydiv vec))

(define (log2 x)
  (inexact->exact (ceiling (/ (log x) (log 2)))))

(define (make-2d-hash w h)
  (define x-buckets (* 4 (log2 w)))
  (define y-buckets (* 4 (log2 h)))
  (define xdiv (ceiling (/ w x-buckets)))
  (define ydiv (ceiling (/ h y-buckets)))
  (define v (make-vector (* x-buckets y-buckets) null))
  (2d-hash x-buckets xdiv y-buckets ydiv v))

(define 2d-hash-x-blocks 2d-hash-xbs)
(define 2d-hash-y-blocks 2d-hash-ybs)

(define (2d-hash-x-block-min ht xb)
  (* (2d-hash-xdiv ht) xb))
(define (2d-hash-x-block-max ht xb)
  (2d-hash-x-block-min ht (add1 xb)))
(define (2d-hash-y-block-min ht yb)
  (* (2d-hash-ydiv ht) yb))
(define (2d-hash-y-block-max ht yb)
  (2d-hash-y-block-min ht (add1 yb)))

(define (2d-hash-clear! ht)
  (vector-fill! (2d-hash-vec ht) null))

(define (vector-cons! vec i v)
  (vector-set! vec i (cons v (vector-ref vec i))))

(define (x-bucket ht x)
  (define r (quotient x (2d-hash-xdiv ht)))
  r)
(define (y-bucket ht y)
  (quotient y (2d-hash-ydiv ht)))

(define (offset ht x y)
  (define r (+ (* (2d-hash-xbs ht) x) y))
  r)

(define (2d-hash-add! ht mx Mx my My v)
  (define-syntax-rule (in-buckets x-bucket mx Mx)
    (in-range (x-bucket ht mx) (add1 (x-bucket ht Mx))))
  (for* ([xb (in-buckets x-bucket mx Mx)]
         [yb (in-buckets y-bucket my My)])
    (vector-cons! (2d-hash-vec ht)
                  (offset ht xb yb)
                  v)))

(define (2d-hash-block-ref ht xb yb)
  (vector-ref (2d-hash-vec ht) (offset ht xb yb)))

(define (2d-hash-ref ht x y)
  (define xb (x-bucket ht x))
  (define yb (y-bucket ht y))
  (2d-hash-block-ref ht xb yb))

(provide
 (contract-out
  [2d-hash?
   (-> any/c
       boolean?)]
  [make-2d-hash
   (-> exact-nonnegative-integer? exact-nonnegative-integer?
       2d-hash?)]
  [2d-hash-x-blocks
   (-> 2d-hash?
       exact-nonnegative-integer?)]
  [2d-hash-x-block-min
   (-> 2d-hash? exact-nonnegative-integer?
       exact-nonnegative-integer?)]
  [2d-hash-x-block-max
   (-> 2d-hash? exact-nonnegative-integer?
       exact-nonnegative-integer?)]
  [2d-hash-y-blocks
   (-> 2d-hash?
       exact-nonnegative-integer?)]
  [2d-hash-y-block-min
   (-> 2d-hash? exact-nonnegative-integer?
       exact-nonnegative-integer?)]
  [2d-hash-y-block-max
   (-> 2d-hash? exact-nonnegative-integer?
       exact-nonnegative-integer?)]
  [2d-hash-clear!
   (-> 2d-hash?
       void?)]
  [2d-hash-add!
   (-> 2d-hash?
       exact-nonnegative-integer? exact-nonnegative-integer?
       exact-nonnegative-integer? exact-nonnegative-integer?
       any/c
       void?)]
  [2d-hash-ref
   (-> 2d-hash?
       exact-nonnegative-integer? exact-nonnegative-integer?
       list?)]
  [2d-hash-block-ref
   (-> 2d-hash?
       exact-nonnegative-integer? exact-nonnegative-integer?
       list?)]))
