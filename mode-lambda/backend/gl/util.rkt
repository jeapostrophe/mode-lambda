#lang racket/base
(require opengl
         racket/match
         web-server/templates
         ffi/vector)

(define (make-delayed-until-gl-is-around t)
  (define c #f)
  (define r void)
  (λ ()
    (unless c
      (set! c #t)
      (set! r (t)))
    r))

(define (make-2dtexture)
  (define Id (glGen glGenTextures))
  (with-texture (GL_TEXTURE0 Id)
    (2D-defaults))
  Id)

(define (load-texture/bytes w h bs)
  (define the-copy (bytes-copy bs))
  (argb->rgba! the-copy)
  (glTexImage2D GL_TEXTURE_2D
                0 GL_RGBA
                w h 0
                GL_RGBA GL_UNSIGNED_BYTE
                the-copy))

(define (load-texture/float-bytes w h bs)
  (glTexImage2D GL_TEXTURE_2D
                0 GL_RGBA32F
                ;; w is in float values, but RGBA32F is 4 float
                ;; values, so we need to do a fourth of this.
                (ceiling (/ w 4)) h 0
                GL_RGBA GL_FLOAT
                bs))

(define (2D-defaults)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE))

(define (gl-texture-index i)
  (match i
    [(== GL_TEXTURE0) 0]
    [(== GL_TEXTURE1) 1]
    [(== GL_TEXTURE2) 2]
    [(== GL_TEXTURE3) 3]))

(define-syntax-rule (wrapper (before ...) (middle ...) (after ...))
  (let () before ... middle ... after ...))

(define-syntax-rule (define-with-state with-state (F static-arg ...))
  (define-syntax-rule (with-state (dyn-arg (... ...)) . body)
    (wrapper [(F static-arg ... dyn-arg (... ...))]
             body
             [(F static-arg ... 0)])))

(define-with-state with-program (glUseProgram))
(define-with-state with-vertexarray (glBindVertexArray))
(define-syntax-rule (with-texture-array (TextureId) . body)
  (wrapper [(glBindTexture GL_TEXTURE_2D_ARRAY TextureId)]
           body
           [(glBindTexture GL_TEXTURE_2D_ARRAY 0)]))
(define-syntax-rule (with-texture (GL_TEXTUREx TextureId) . body)
  (wrapper [(glActiveTexture GL_TEXTUREx)
            (glBindTexture GL_TEXTURE_2D TextureId)]
           body
           [(glActiveTexture GL_TEXTUREx)
            (glBindTexture GL_TEXTURE_2D 0)]))
(define-with-state with-framebuffer (glBindFramebuffer GL_FRAMEBUFFER))
(define-with-state with-renderbuffer (glBindRenderbuffer GL_RENDERBUFFER))
(define-with-state with-arraybuffer (glBindBuffer GL_ARRAY_BUFFER))

(define-syntax-rule (with-feature (FLAG) . body)
  (wrapper [(glEnable FLAG)] body [(glDisable FLAG)]))

(define-syntax-rule (with-vertex-attributes (AttributeCount) . body)
  (wrapper [(for ([i (in-range AttributeCount)])
              (glEnableVertexAttribArray i))]
           body
           [(for ([i (in-range AttributeCount)])
              (glDisableVertexAttribArray i))]))

(define (glGen glGenThing)
  (u32vector-ref (glGenThing 1) 0))

(define-syntax-rule (define-shader-source id path)
  (define id (include-template path)))

(define (print-shader-log glGetShaderInfoLog shader-name shader-id)
  (define-values (infoLen infoLog)
    (glGetShaderInfoLog shader-id 1024))
  (unless (zero? infoLen)
    (eprintf "Log of shader(~a):\n~a\n"
             shader-name
             (subbytes infoLog 0 infoLen))
    (eprintf "Exiting...\n")
    (exit 1)))

(define-syntax-rule
  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId VertexShader)
  (begin (define VertexShaderId (glCreateShader GL_VERTEX_SHADER))
         (glShaderSource VertexShaderId 1 (vector VertexShader)
                         (s32vector))
         (glCompileShader VertexShaderId)
         (print-shader-log glGetShaderInfoLog 'VertexShader VertexShaderId)
         (glAttachShader ProgramId VertexShaderId)))

;; COPIED FROM opengl/main
;; Convert argb -> rgba
;; Modern wisdom is not to convert to rgba but rather use
;; GL_BGRA with GL_UNSIGNED_INT_8_8_8_8_REV. But that turns out not
;; to work on some implementations, even ones which advertise
;; OpenGL 1.2 support. Great.
(define (argb->rgba! pixels)
  (for ((i (in-range (/ (bytes-length pixels) 4))))
    (let* ((offset (* 4 i))
           (alpha (bytes-ref pixels (+ 0 offset)))
           (  red (bytes-ref pixels (+ 1 offset)))
           (green (bytes-ref pixels (+ 2 offset)))
           ( blue (bytes-ref pixels (+ 3 offset))))
      (bytes-set! pixels (+ 0 offset) red)
      (bytes-set! pixels (+ 1 offset) green)
      (bytes-set! pixels (+ 2 offset) blue)
      (bytes-set! pixels (+ 3 offset) alpha))))
;; </COPIED>

(provide
 (all-defined-out))
