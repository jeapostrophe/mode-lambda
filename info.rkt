#lang info
(define collection 'multi)
(define deps '("gui-lib"
               "scheme-lib"
               "web-server-lib"
               "lux"
               "reprovide-lang-lib"
               "base" "srfi-lite-lib" "draw-lib" "opengl" "htdp-lib" "pict-lib" "draw-lib"))
(define build-deps '("draw-doc"
                     "racket-doc"
                     "scribble-lib"
                     "rackunit-lib"))
