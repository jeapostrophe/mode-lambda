#lang racket/base
(require (only-in ffi/unsafe
                  ctype-sizeof
                  _float)
         racket/fixnum)

(define INDEX-VALUES 4)
(define (sprite-index->bytes idx->w*h*tx*ty)
  (define sprite-index-count
    (vector-length idx->w*h*tx*ty))
  (define index-bytes-per-value (ctype-sizeof _float))
  (define index-bin
    (make-bytes (fx* (fx* INDEX-VALUES index-bytes-per-value)
                     sprite-index-count)))
  (for ([vec (in-vector idx->w*h*tx*ty)]
        [i (in-naturals)])
    (for ([v (in-vector vec)]
          [o (in-naturals)])
      (real->floating-point-bytes
       v index-bytes-per-value
       (system-big-endian?) index-bin
       (fx+ (fx* (fx* INDEX-VALUES
                      index-bytes-per-value)
                 i)
            (fx* index-bytes-per-value o)))))
  index-bin)

(provide (all-defined-out))
