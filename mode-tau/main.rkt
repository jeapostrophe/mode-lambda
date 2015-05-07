#lang racket/base
(require racket/fixnum
         racket/match
         racket/flonum
         mode-lambda/text
         mode-lambda/util
         mode-lambda/core
         (only-in mode-lambda
                  make-sprite-db
                  compile-sprite-db
                  sprite-width
                  sprite-height)
         mode-lambda/sprite-index
         mode-lambda/backend/gl/util
         (except-in ffi/unsafe ->)
         ffi/cvector)
;;

(define (make-glyph-db)
  (make-sprite-db))
(define (compile-glyph-db sd)
  (compile-sprite-db sd))
(define (font-glyph-idx the-font cgdb char)
  (font-char-idx the-font cgdb char))
(define (glyph-width sd ci)
  (fx->fl (sprite-width sd ci)))
(define (glyph-height sd ci)
  (fx->fl (sprite-height sd ci)))

;;

(define gui-mode 'gl-core)

(define VertsPerGlyph (fx* 2 3))

(define-cstruct&list
  _glyph-data _glyph-data:info
  ([dx _float]     ;; 0    0

   [dy _float]     ;; 1    4

   [glyph _uint]   ;; 2    8

   [fgr _byte]     ;; 3   12
   [fgg _byte]     ;; 4   13
   [fgb _byte]     ;; 5   14
   [bgr _byte]     ;; 6   15

   [bgg _byte]     ;; 7   16
   [bgb _byte]     ;; 8   17
   [horiz _int8]   ;; 9   18
   [vert _int8]    ;; 10  19
   ))

(define (glyph dx dy glyph-idx
               ;; xxx add COLOR arg
               #:fgr [fgr 0] #:fgb [fgb 0] #:fgg [fgg 0]
               ;; xxx add COLOR arg
               #:bgr [bgr 255] #:bgb [bgb 255] #:bgg [bgg 255])
  ;; FIXME figure out how to abstract this hack from mode-lambda/main
  (define v (make-cvector _glyph-data VertsPerGlyph))
  (define o (make-glyph-data dx dy glyph-idx fgr fgg fgb bgr bgg bgb -1 +1))
  (define-syntax-rule (point-install! (which ...) o)
    (begin (memcpy (cvector-ptr v) which o 1 _glyph-data)
           ...))
  ;; -1 +1
  (point-install! (0) o)
  ;; +1 +1
  (set-glyph-data-horiz! o +1)
  (point-install! (1 4) o)
  ;; +1 -1
  (set-glyph-data-vert! o -1)
  (point-install! (5) o)
  ;; -1 -1
  (set-glyph-data-horiz! o -1)
  (point-install! (2 3) o)

  v)

(define (make-render cgdb)
  (local-require opengl
                 scheme/nest)

  (match-define
    (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                        pal-size pal-bs pal->idx)
    cgdb)

  (define SpriteAtlasId (make-2dtexture))
  (with-texture (GL_TEXTURE0 SpriteAtlasId)
    ;; (eprintf "Uploading ~v kb to GPU\n" (/ (* atlas-size atlas-size) 1024))
    (load-texture/bytes atlas-size atlas-size atlas-bs))

  (define SpriteIndexId (make-2dtexture))
  (with-texture (GL_TEXTURE0 SpriteIndexId)
    (load-texture/float-bytes
     INDEX-VALUES (vector-length idx->w*h*tx*ty)
     (sprite-index->bytes idx->w*h*tx*ty)))

  (define tau-program (glCreateProgram))
  (bind-attribs/cstruct-info tau-program _glyph-data:info)

  (define-shader-source tau-vert "tau.vertex.glsl")
  (define-shader-source tau-fragment "tau.fragment.glsl")

  (compile-shader GL_VERTEX_SHADER tau-program tau-vert)
  (compile-shader GL_FRAGMENT_SHADER tau-program tau-fragment)

  (glLinkProgram&check tau-program)
  (with-program (tau-program)
    (glUniform1i (glGetUniformLocation tau-program "SpriteAtlasTex")
                 (gl-texture-index GL_TEXTURE0))
    (glUniform1i (glGetUniformLocation tau-program "SpriteIndexTex")
                 (gl-texture-index GL_TEXTURE1)))

  (define tau-vao (glGen glGenVertexArrays))
  (define tau-vbo (glGen glGenBuffers))
  (nest
   ([with-vertexarray (tau-vao)]
    [with-arraybuffer (tau-vbo)])
   (define-attribs/cstruct-info _glyph-data:info))

  (define update!
    (make-update-vbo-buffer-with-objects! VertsPerGlyph _glyph-data tau-vbo))

  (λ (w h bgr bgg bgb glyphs)
    (define obj-count (update! glyphs))
    (nest
     ([with-texture (GL_TEXTURE0 SpriteAtlasId)]
      [with-texture (GL_TEXTURE1 SpriteIndexId)]
      [with-program (tau-program)])
     (glClearColor (fl/ (fx->fl bgr) 255.0)
                   (fl/ (fx->fl bgg) 255.0)
                   (fl/ (fx->fl bgb) 255.0) 1.0)
     (glClear GL_COLOR_BUFFER_BIT)
     (glViewport 0 0 w h)
     (set-uniform-fpair! tau-program "Viewport"
                         (pair->fpair (fx->fl w) (fx->fl h)))
     (nest
      ([with-vertexarray (tau-vao)]
       [with-vertex-attributes ((length _glyph-data:info))])
      (glDrawArrays GL_TRIANGLES 0 (fx* VertsPerGlyph obj-count))))))

(define-make-delayed-render
  stage-render
  make-render
  (gdb) ()
  (bgr bgg bgb glyphs))

(provide stage-render
         glyph
         make-glyph-db
         load-font!
         compile-glyph-db
         font-glyph-idx
         glyph-width
         glyph-height
         gui-mode)
