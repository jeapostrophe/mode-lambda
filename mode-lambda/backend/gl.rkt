#lang racket/base

(require ffi/cvector
         ffi/unsafe/cvector
         ffi/vector
         mode-lambda/backend/lib
         mode-lambda/core
         racket/class
         racket/contract
         racket/draw
         racket/gui/base
         racket/file
         racket/function
         racket/list
         racket/match
         racket/runtime-path
         opengl
         (only-in math/base
                  sum)
         (only-in ffi/unsafe
                  ctype-sizeof
                  ctype->layout
                  define-cstruct
                  _float
                  _sint32
                  _uint32
                  _sint16
                  _uint16
                  _sint8
                  _uint8))

(define (make-delayed-until-gl-is-around t)
  (define c #f)
  (define r #f)
  (λ ()
    (unless c
      (set! c #t)
      (set! r (t)))
    r))

;; Old Code from GB (gl-util.rkt)

(define-syntax-rule (define-shader-source id path)
  (begin (define-runtime-path id-path path)
         (define id (file->string id-path))))

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

;; Old Code from GB (crt.rkt)

(define-shader-source crt-fragment "gl/crt.fragment.glsl")
(define-shader-source crt-vert "gl/crt.vertex.glsl")
(define-shader-source std-fragment "gl/std.fragment.glsl")
(define-shader-source std-vert "gl/std.vertex.glsl")

(define (quotient* x y)
  (define-values (q r) (quotient/remainder x y))
  (define (recur r i max-i)
    (cond
     [(= i max-i)
      0]
     [else
      (define d (expt 2 (* -1 i)))
      (define dy (* d y))
      (cond
       [(> dy r)
        (recur r (add1 i) max-i)]
       [else
        (+ d (recur (- r dy) (add1 i) max-i))])]))
  (+ q (recur r 1 5)))

(define (make-draw-on-crt crt-width crt-height mode)
  (eprintf "You are using OpenGL ~a\n"
           (gl-version))

  (define texture-width crt-width)
  (define texture-height crt-height)

  (define myTexture (u32vector-ref (glGenTextures 1) 0))

  (glBindTexture GL_TEXTURE_2D myTexture)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  (printf "crt texture prep\n")
  (glTexImage2D
   GL_TEXTURE_2D 0 GL_RGBA8 texture-width texture-height 0
   GL_RGBA GL_UNSIGNED_BYTE
   0)
  (glBindTexture GL_TEXTURE_2D 0)

  (define myRB (u32vector-ref (glGenRenderbuffers 1) 0))

  (glBindRenderbuffer GL_RENDERBUFFER myRB)
  (glRenderbufferStorage GL_RENDERBUFFER
                         GL_DEPTH_COMPONENT24
                         texture-width texture-height)
  (glBindRenderbuffer GL_RENDERBUFFER 0)

  (define myFBO (u32vector-ref (glGenFramebuffers 1) 0))

  (glBindFramebuffer GL_FRAMEBUFFER myFBO)
  (glFramebufferTexture2D
   GL_DRAW_FRAMEBUFFER
   GL_COLOR_ATTACHMENT0
   GL_TEXTURE_2D myTexture 0)

  (glFramebufferRenderbuffer
   GL_FRAMEBUFFER
   GL_DEPTH_ATTACHMENT
   GL_RENDERBUFFER myRB)

  (match (glCheckFramebufferStatus GL_FRAMEBUFFER)
    [(== GL_FRAMEBUFFER_COMPLETE)
     (void)]
    [x
     (eprintf "FBO creation failed: ~v\n" x)
     (exit 1)])

  (glBindFramebuffer GL_FRAMEBUFFER 0)

  (define shader_program (glCreateProgram))
  (glBindAttribLocation shader_program 0 "iTexCoordPos")

  (define-values (the-fragment the-vert)
    (match mode
      ['crt (values crt-fragment crt-vert)]
      ['std (values std-fragment std-vert)]))

  (define&compile-shader fragment_shader GL_FRAGMENT_SHADER
    shader_program the-fragment)
  (define&compile-shader vertex_shader GL_VERTEX_SHADER
    shader_program the-vert)

  (glLinkProgram shader_program)
  (print-shader-log glGetProgramInfoLog 'Program shader_program)

  (glUseProgram shader_program)

  (glUniform1i
   (glGetUniformLocation shader_program "rubyTexture")
   0)
  (glUniform2fv
   (glGetUniformLocation shader_program "rubyInputSize")
   1
   (f32vector (* 1. crt-width) (* 1. crt-height)))
  (glUniform2fv
   (glGetUniformLocation shader_program "rubyTextureSize")
   1
   (f32vector (* 1. texture-width) (* 1. texture-height)))

  (glUseProgram 0)

  (define VaoId (u32vector-ref (glGenVertexArrays 1) 0))
  (define VboId (u32vector-ref (glGenBuffers 1) 0))

  (define (new-draw-on-crt actual-screen-width actual-screen-height do-the-drawing)
    ;; Init

    (define scale
      (* 1.
         (min (quotient* actual-screen-width crt-width)
              (quotient* actual-screen-height crt-height))))

    (define screen-width (* scale crt-width))
    (define screen-height (* scale crt-height))

    (define inset-left (/ (- actual-screen-width screen-width) 2.))
    (define inset-right (+ inset-left screen-width))
    (define inset-bottom (/ (- actual-screen-height screen-height) 2.))
    (define inset-top (+ inset-bottom screen-height))

    (glUseProgram shader_program)
    (glUniform2fv
     (glGetUniformLocation shader_program "rubyOutputSize")
     1
     (f32vector (* 1. actual-screen-width) (* 1. actual-screen-height)))
    (glUseProgram 0)
    (glBindVertexArray VaoId)
    (glBindBuffer GL_ARRAY_BUFFER VboId)

    (define DataWidth 4)
    (define DataSize 4)
    (define DataCount 6)
    (glVertexAttribPointer 0 DataSize GL_FLOAT #f 0 0)
    (glEnableVertexAttribArray 0)

    (glBufferData GL_ARRAY_BUFFER (* DataCount DataWidth DataSize) #f GL_STATIC_DRAW)

    (define DataVec
      (make-cvector*
       (glMapBufferRange
        GL_ARRAY_BUFFER
        0
        (* DataCount DataSize)
        GL_MAP_WRITE_BIT)
       _float
       (* DataWidth
          DataSize
          DataCount)))
    (define (cvector-set*! vec k . vs)
      (for ([v (in-list vs)]
            [i (in-naturals)])
        (cvector-set! vec (+ k i) v)))
    (cvector-set*! DataVec 0
                   0.0 0.0 inset-left inset-bottom
                   1.0 0.0 inset-right inset-bottom
                   1.0 1.0 inset-right inset-top

                   0.0 1.0 inset-left inset-top
                   1.0 1.0 inset-right inset-top
                   0.0 0.0 inset-left inset-bottom)
    (glUnmapBuffer GL_ARRAY_BUFFER)
    (set! DataVec #f)

    (glBindBuffer GL_ARRAY_BUFFER 0)
    (glBindVertexArray 0)

    ;; Draw
    (glBindFramebuffer GL_FRAMEBUFFER myFBO)
    (glViewport 0 0 crt-width crt-height)
    (do-the-drawing)
    (glBindFramebuffer GL_FRAMEBUFFER 0)

    (glBindVertexArray VaoId)
    (glEnableVertexAttribArray 0)
    (glUseProgram shader_program)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (glViewport 0 0 actual-screen-width actual-screen-height)
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D myTexture)
    (glDrawArrays GL_TRIANGLES 0 DataCount)

    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D 0)
    (glUseProgram 0)
    (glDisableVertexAttribArray 0)
    (glBindVertexArray 0))

  new-draw-on-crt)

;; Old Code from GB (ngl.rkt)

(define (num->pow2 n)
  (integer-length n))

(define debug? #f)

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

(define ctype-name->bytes
  (match-lambda
   ['uint8 1]
   ['int8 1]
   ['int16 2]
   ['uint16 2]
   ['int32 4]
   ['uint32 4]
   ['float 4]))
(define (ctype-offset _type offset)
  (sum (map ctype-name->bytes (take (ctype->layout _type) offset))))

(define (sublist l s e)
  (for/list ([x (in-list l)]
             [i (in-naturals)]
             #:when (<= s i)
             #:when (<= i e))
    x))

(define (list-only l)
  (define v (first l))
  (for ([x (in-list (rest l))])
    (unless (eq? v x) (error 'list-only "List is not uniform: ~e" l)))
  v)

(define (ctype-range-type _type s e)
  (list-only (sublist (ctype->layout _type) s e)))

(define ctype->gltype
  (match-lambda
   ['uint8 (values #t GL_UNSIGNED_BYTE)]
   ['int8 (values #t GL_BYTE)]
   ['uint16 (values #t GL_UNSIGNED_SHORT)]
   ['int16 (values #t GL_SHORT)]
   ['uint32 (values #t GL_UNSIGNED_INT)]
   ['int32 (values #t GL_INT)]
   ['float (values #f GL_FLOAT)]))

;; xxx everything about layers is ignored
(define-shader-source ngl-vert "gl/ngl.vertex.glsl")
(define-shader-source ngl-fragment "gl/ngl.fragment.glsl")

(define DrawnMult 6)

(define (make-draw csd width height)
  (match-define
   (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                       pal-size pal-bs pal->idx)
   csd)

  (define SpriteData-count 0)
  (define SpriteData #f)

  (define (install-object! i o)
    (define-syntax-rule (point-install! Horiz Vert j ...)
      (begin
        (set-sprite-data-horiz! o Horiz)
        (set-sprite-data-vert! o Vert)
        (cvector-set! SpriteData (+ (* i 6) j) o)
        ...))
    (point-install! -1 +1 0)
    (point-install! +1 +1 1 4)
    (point-install! -1 -1 2 3)
    (point-install! +1 -1 5))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))
  (glBindAttribLocation ProgramId 0 "in_DX_DY")
  (glBindAttribLocation ProgramId 1 "in_MX_MY_THETA_A")
  (glBindAttribLocation ProgramId 2 "in_SPR_PAL")
  (glBindAttribLocation ProgramId 3 "in_LAYER_R_G_B")
  (glBindAttribLocation ProgramId 4 "in_HORIZ_VERT")

  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId ngl-vert)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId ngl-fragment)

  (define DrawType GL_TRIANGLES)
  (define AttributeCount 5)
  (define *initialize-count* (* 2 512))

  (define (install-objects! t)
    (tree-fold (λ (offset o)
                 (install-object! offset o)
                 (add1 offset))
               0 t))
  (define (count-objects t)
    (tree-fold (λ (count o) (add1 count)) 0 t))

  (define (2D-defaults)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))

  (define (make-texture/bytes w h bs)
    (define Id (u32vector-ref (glGenTextures 1) 0))
    (glBindTexture GL_TEXTURE_2D Id)
    (2D-defaults)
    (define the-copy (bytes-copy bs))
    (argb->rgba! the-copy)
    (glTexImage2D GL_TEXTURE_2D
                  0 GL_RGBA
                  w h 0
                  GL_RGBA GL_UNSIGNED_BYTE
                  the-copy)
    Id)

  (define SpriteAtlasId (make-texture/bytes atlas-size atlas-size atlas-bs))
  (define PaletteAtlasId (make-texture/bytes PALETTE-DEPTH pal-size pal-bs))
  (define SpriteIndexId (u32vector-ref (glGenTextures 1) 0))
  (let ()
    (glBindTexture GL_TEXTURE_2D SpriteIndexId)
    (2D-defaults)
    (define sprite-index-count
      (vector-length idx->w*h*tx*ty))

    (define index-values 4)
    (define index-bytes-per-value 4)
    (define index-bin
      (make-bytes (* index-values index-bytes-per-value
                     sprite-index-count)))

    (for ([vec (in-vector idx->w*h*tx*ty)]
          [i (in-naturals)])
      (for ([v (in-vector vec)]
            [o (in-naturals)])
        (real->floating-point-bytes
         v index-bytes-per-value
         (system-big-endian?) index-bin
         (+ (* index-values
               index-bytes-per-value
               i)
            (* index-bytes-per-value o)))))

    (define sic-width
      (num->pow2 sprite-index-count))
    (define effective-sprite-index-count
      (expt 2 sic-width))
    (glTexImage2D GL_TEXTURE_2D
                  0 GL_RGBA32F
                  1 effective-sprite-index-count 0
                  GL_RGBA GL_FLOAT
                  index-bin))

  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)

  (glUseProgram ProgramId)
  (glUniform1i (glGetUniformLocation ProgramId "SpriteAtlasTex")
               0)
  (glUniform1i (glGetUniformLocation ProgramId "PaletteAtlasTex")
               1)
  (glUniform1i (glGetUniformLocation ProgramId "SpriteIndexTex")
               2)
  (glUniform1ui (glGetUniformLocation ProgramId "ViewportWidth")
                width)
  (glUniform1ui (glGetUniformLocation ProgramId "ViewportHeight")
                height)
  (glUseProgram 0)

  (define VaoId (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (define (glVertexAttribIPointer* index size type normalized stride pointer)
    (glVertexAttribIPointer index size type stride pointer))

  (define-syntax-rule
    (define-vertex-attrib-array
      Index SpriteData-start SpriteData-end)
    (begin
      (define-values (int? type)
        (ctype->gltype (ctype-range-type _sprite-data SpriteData-start SpriteData-end)))
      (define byte-offset
        (ctype-offset _sprite-data SpriteData-start))
      (define HowMany
        (add1 (- SpriteData-end SpriteData-start)))
      (when debug?
        (eprintf "~v\n"
                 `(glVertexAttribPointer
                   ,Index ,HowMany ,type
                   #f
                   ,(ctype-sizeof _sprite-data)
                   ,byte-offset)))
      ((if int? glVertexAttribIPointer* glVertexAttribPointer)
       Index HowMany type
       #f
       (ctype-sizeof _sprite-data)
       byte-offset)
      (glEnableVertexAttribArray Index)))

  (define VboId
    (u32vector-ref (glGenBuffers 1) 0))

  (glBindBuffer GL_ARRAY_BUFFER VboId)

  (define-syntax-rule
    (define-vertex-attrib-array*
      [AttribId AttribStart AttribEnd] ...)
    (begin
      (define-vertex-attrib-array AttribId AttribStart AttribEnd)
      ...))

  (define-vertex-attrib-array*
    [0  0  1]
    [1  2  5]
    [2  6  7]
    [3  8 11]
    [4 12 13])

  (glBindBuffer GL_ARRAY_BUFFER 0)

  (glBindVertexArray 0)

  (define (draw objects)
    (glBindVertexArray VaoId)

    (for ([i (in-range AttributeCount)])
      (glEnableVertexAttribArray i))

    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D SpriteAtlasId)
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D PaletteAtlasId)
    (glActiveTexture GL_TEXTURE2)
    (glBindTexture GL_TEXTURE_2D SpriteIndexId)

    (glBindBuffer GL_ARRAY_BUFFER VboId)

    ;; xxx can i optimize this by copying into a different (or the
    ;; same) c buffer and then do a big memcpy?
    (define early-count (count-objects objects))
    (when debug?
      (printf "early count is ~a\n" early-count))
    (define SpriteData-count:new (max *initialize-count* early-count))

    (unless (>= SpriteData-count SpriteData-count:new)
      (define SpriteData-count:old SpriteData-count)
      (set! SpriteData-count
            (max (* 2 SpriteData-count)
                 SpriteData-count:new))
      (when debug?
        (printf "~a -> max(~a,~a) = ~a\n"
                SpriteData-count:old
                (* 2 SpriteData-count)
                SpriteData-count:new
                SpriteData-count))
      (glBufferData GL_ARRAY_BUFFER
                    (* SpriteData-count
                       DrawnMult
                       (ctype-sizeof _sprite-data))
                    #f
                    GL_STREAM_DRAW))

    (set! SpriteData
          (make-cvector*
           (glMapBufferRange
            GL_ARRAY_BUFFER
            0
            (* SpriteData-count
               DrawnMult
               (ctype-sizeof _sprite-data))
            (bitwise-ior
             ;; We are overriding everything (this would be wrong if
             ;; we did the caching "optimization" I imagine)
             GL_MAP_INVALIDATE_RANGE_BIT
             GL_MAP_INVALIDATE_BUFFER_BIT

             ;; We are not doing complex queues, so don't block other
             ;; operations (but it doesn't seem to improve performance
             ;; by having this option)
             ;; GL_MAP_UNSYNCHRONIZED_BIT

             ;; We are writing
             GL_MAP_WRITE_BIT))
           _sprite-data
           (* SpriteData-count
              DrawnMult)))

    ;; Reload all data every frame
    (install-objects! objects)
    (define this-count early-count)
    (glUnmapBuffer GL_ARRAY_BUFFER)
    (glBindBuffer GL_ARRAY_BUFFER 0)

    (glUseProgram ProgramId)

    (glEnable GL_DEPTH_TEST)
    (glClearColor 0.0 0.0 0.0 0.0)

    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

    (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

    (define drawn-count this-count)
    (glDrawArrays
     DrawType 0
     (* DrawnMult drawn-count))

    (glDisable GL_DEPTH_TEST)
    (glDisable GL_BLEND)

    ;; This is actually already active
    (glActiveTexture GL_TEXTURE2)
    (glBindTexture GL_TEXTURE_2D 0)
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D 0)
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D 0)

    (for ([i (in-range AttributeCount)])
      (glDisableVertexAttribArray i))

    (glBindVertexArray 0)

    (glUseProgram 0))

  draw)

;; New Interface

(define (stage-draw/dc csd width height)
  (define draw-on-crt
    (make-delayed-until-gl-is-around
     (λ ()
       (make-draw-on-crt width height 'std))))
  (define draw-sprites
    (make-delayed-until-gl-is-around
     (λ ()
       (make-draw csd width height))))
  (λ (layer-config static-st dynamic-st)
    ;; xxx send this through
    (define sprite-tree (cons static-st dynamic-st))
    (λ (w h dc)
      (define glctx (send dc get-gl-context))
      (unless glctx
        (error 'draw "Could not initialize OpenGL!"))
      (send glctx call-as-current
            (λ ()
              ((draw-on-crt) w h
               (λ () ((draw-sprites) sprite-tree)))
              (send glctx swap-buffers))))))

(define gui-mode 'gl-core)
(provide
 (contract-out
  [gui-mode symbol?]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
