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
  (define&compile-shader VertexShaderId
    GL_VERTEX_SHADER
    ProgramId VertexShader)
  (begin (define VertexShaderId (glCreateShader GL_VERTEX_SHADER))
         (glShaderSource VertexShaderId 1 (vector VertexShader)
                         (s32vector))
         (glCompileShader VertexShaderId)
         (print-shader-log glGetShaderInfoLog 'VertexShader VertexShaderId)
         (glAttachShader ProgramId VertexShaderId)))

;; Old Code from GB (crt.rkt)

;; xxx simplify this
(define-shader-source fragment-source "gl/crt.fragment.glsl")
;; xxx simplify this
(define-shader-source vertex-source "gl/crt.vertex.glsl")

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

(define (make-draw-on-crt crt-width crt-height)
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

  (define&compile-shader fragment_shader
    GL_FRAGMENT_SHADER
    shader_program fragment-source)

  (define&compile-shader vertex_shader
    GL_VERTEX_SHADER
    shader_program vertex-source)

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
    (glClearColor 0. 0. 0. 0.)
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

(define-cstruct _sprite-info
  ([x _float]     ;; 0
   [y _float]     ;; 1
   [hw _float]    ;; 2
   [hh _float]    ;; 3
   [r _uint8]     ;; 4
   [g _uint8]     ;; 5
   [b _uint8]     ;; 6
   [a _uint8]     ;; 7
   [mx _float]    ;; 8
   [my _float]    ;; 9
   [theta _float] ;; 10

   ;; This is a hack because we need to ensure we are aligned for
   ;; OpenGL, so we're ignoring _palette and _sprite-index. At this
   ;; moment, pal is "too" large and spr is just right. When we have
   ;; more than 65k palettes or 65k sprites, there will be a
   ;; problem. (BTW, because of normal alignment, if we change pal to
   ;; just be a byte, it will still take up the same amount of space
   ;; total.)
   [pal _uint16]   ;; 11
   [spr _uint16]   ;; 12

   [horiz _sint8]  ;; 13
   [vert _sint8])) ;; 14

(define (create-sprite-info x y hw hh r g b a spr pal mx my theta)
  (make-sprite-info x y hw hh
                    r g b a
                    mx my theta
                    pal spr
                    0 0))

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

;; xxx simplify this
(define-shader-source VertexShader "gl/ngl.vertex.glsl")
;; xxx simplify this
(define-shader-source FragmentShader "gl/ngl.fragment.glsl")

(define DrawnMult 6)

(define (make-draw sprite-atlas-bytes
                   sprite-index-data
                   palette-atlas-bs
                   width height)
  (define SpriteData-count
    0)
  (define SpriteData #f)

  (define (install-object! i o)
    (define-syntax-rule (point-install! Horiz Vert j ...)
      (begin
        (set-sprite-info-horiz! o Horiz)
        (set-sprite-info-vert! o Vert)
        (cvector-set! SpriteData (+ (* i 6) j) o)
        ...))
    ;; I once thought I could use a degenerative triangle strip, but
    ;; that adds 2 additional vertices on all but the first and last
    ;; triangles, which would save me exactly 2 vertices total.
    (point-install! -1 +1 0)
    (point-install! +1 +1 1 4)
    (point-install! -1 -1 2 3)
    (point-install! +1 -1 5))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))
  (glBindAttribLocation ProgramId 0 "in_Position")
  (glBindAttribLocation ProgramId 1 "in_iColor")
  (glBindAttribLocation ProgramId 2 "in_iTexIndex")
  (glBindAttribLocation ProgramId 3 "in_Transforms")
  (glBindAttribLocation ProgramId 4 "in_iVertexSpecification")
  (glBindAttribLocation ProgramId 5 "in_iPalette")

  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId VertexShader)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId FragmentShader)

  (define DrawType GL_TRIANGLES)
  (define AttributeCount 6)

  (define *initialize-count*
    (* 2 512))

  (define (install-objects! t)
    (tree-fold
     (λ (offset o)
       (install-object! offset o)
       (add1 offset))
     0
     t))
  (define (count-objects t)
    (tree-fold
     (λ (count o)
       (add1 count))
     0
     t))

  (define (2D-defaults)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))

  (define SpriteAtlasId (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D SpriteAtlasId)
  (2D-defaults)
  (let ()
    (define sprite-atlas-size (sqrt (/ (bytes-length sprite-atlas-bytes) 4)))
    (printf "loading sprite atlas texture\n")
    (argb->rgba! sprite-atlas-bytes)
    ;; xxx this needs to be updated for full color
    (glTexImage2D GL_TEXTURE_2D
                  0 GL_RGBA
                  sprite-atlas-size sprite-atlas-size 0
                  GL_RGBA GL_UNSIGNED_BYTE
                  sprite-atlas-bytes))

  (define PaletteAtlasId (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D PaletteAtlasId)
  (2D-defaults)
  (let ()
    (printf "loading palette atlas texture\n")
    (argb->rgba! palette-atlas-bs)
    (glTexImage2D GL_TEXTURE_2D
                  0 GL_RGBA
                  PALETTE-DEPTH
                  (/ (bytes-length palette-atlas-bs)
                     (* 4 PALETTE-DEPTH))
                  0
                  GL_RGBA GL_UNSIGNED_BYTE
                  palette-atlas-bs))

  (define SpriteIndexId (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D SpriteIndexId)
  (2D-defaults)
  (let ()
    (define sprite-index-bytes 4)
    (define sprite-index-count (/ (bytes-length sprite-index-data)
                                  (* 4 sprite-index-bytes)))
    (define sic-width (num->pow2 sprite-index-count))
    (define effective-sprite-index-count
      (expt 2 sic-width))
    (printf "loading sprite index texture: ~v\n"
            (vector sprite-index-count
                    sic-width
                    effective-sprite-index-count))
    (glTexImage2D GL_TEXTURE_2D
                  0 GL_RGBA32F
                  1 effective-sprite-index-count 0
                  GL_RGBA GL_FLOAT
                  sprite-index-data))

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

  ;; Create VBOs
  (define VaoId
    (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (define (glVertexAttribIPointer* index size type normalized stride pointer)
    (glVertexAttribIPointer index size type stride pointer))

  (define-syntax-rule
    (define-vertex-attrib-array
      Index SpriteData-start SpriteData-end)
    (begin
      (define-values (int? type)
        (ctype->gltype (ctype-range-type _sprite-info SpriteData-start SpriteData-end)))
      (define byte-offset
        (ctype-offset _sprite-info SpriteData-start))
      (define HowMany
        (add1 (- SpriteData-end SpriteData-start)))
      (when debug?
        (eprintf "~v\n"
                 `(glVertexAttribPointer
                   ,Index ,HowMany ,type
                   #f
                   ,(ctype-sizeof _sprite-info)
                   ,byte-offset)))
      ((if int? glVertexAttribIPointer* glVertexAttribPointer)
       Index HowMany type
       #f
       (ctype-sizeof _sprite-info)
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
    [0  0  3] ;; x--hh
    [1  4  7] ;; r--a
    [2 12 12] ;; spr
    [3  8 10] ;; mx--theta
    [4 13 14] ;; horiz--vert
    [5 11 11]) ;; pal

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
                       (ctype-sizeof _sprite-info))
                    #f
                    GL_STREAM_DRAW))

    (set! SpriteData
          (make-cvector*
           (glMapBufferRange
            GL_ARRAY_BUFFER
            0
            (* SpriteData-count
               DrawnMult
               (ctype-sizeof _sprite-info))
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
           _sprite-info
           (* SpriteData-count
              DrawnMult)))

    ;; Reload all data every frame
    (install-objects! objects)
    (define this-count early-count)
    (glUnmapBuffer GL_ARRAY_BUFFER)
    (glBindBuffer GL_ARRAY_BUFFER 0)

    (glUseProgram ProgramId)

    (glEnable GL_DEPTH_TEST)
    (glClearColor 1.0 1.0 1.0 0.0)

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

;; Old copied stuff from GB (apse/compile.rkt)

(define (pixel-ref bs w h bx by i)
  (bytes-ref bs (+ (* (* 4 w) by) (+ (* 4 bx) i))))

(define (compile-static-stuff csd)
  (match-define
   (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                       pal-size pal-bs pal->idx)
   csd)

  (define-values
    (texture-atlas
     texture-index)
    (let ()
      (define atlas-bin
        (make-bytes (* atlas-size atlas-size 4)))

      (define index-values 4)
      (define index-bytes-per-value 4)
      (define index-bin
        (make-bytes (* index-values index-bytes-per-value
                       (vector-length idx->w*h*tx*ty))))

      (for/list ([vec (in-vector idx->w*h*tx*ty)]
                 [i (in-naturals)])
        (match-define (vector w h tx ty) vec)

        (for* ([x (in-range w)]
               [y (in-range h)])
          (bytes-set! atlas-bin
                      (+ (* atlas-size 4 (+ ty y)) (* 4 (+ tx x)) 1)
                      (pixel-ref atlas-bs atlas-size #f (+ tx x) (+ ty y) 2)))

        (for ([v (in-list (list tx ty w h))]
              [o (in-naturals)])
          (real->floating-point-bytes
           v index-bytes-per-value
           (system-big-endian?) index-bin
           (+ (* index-values
                 index-bytes-per-value
                 i)
              (* index-bytes-per-value o)))))

      (values atlas-bin
              index-bin)))

  ;; Make index
  (values texture-atlas
          texture-index
          pal-bs))

(define (convert-sprites csd sprite-tree)
  (define idx->w*h*tx*ty (compiled-sprite-db-idx->w*h*tx*ty csd))
  (define (convert-sprite t)
    (match-define
     (sprite-data dx dy mx my theta a.0 spr-idx pal-idx layer r g b horiz vert)
     t)
    (match-define (vector w h tx ty) (vector-ref idx->w*h*tx*ty spr-idx))
    (define a (inexact->exact (round (* a.0 255))))
    (define hw (* 0.5 w))
    (define hh (* 0.5 h))
    (create-sprite-info dx dy hw hh r g b a spr-idx pal-idx mx my theta))
  (define (f acc t)
    (cons (convert-sprite t) acc))
  (tree-fold f empty sprite-tree))

;; New Interface

(define (stage-draw/dc csd width height)
  ;; xxx don't do this, just use what csd contains
  (define-values (texture-atlas texture-index palette-atlas)
    (compile-static-stuff csd))
  (define draw-on-crt-b (box #f))
  (define draw-sprites-b (box #f))
  (λ (layer-config sprite-tree)
    (define last-sprites
      ;; xxx don't convert
      (convert-sprites csd sprite-tree))
    ;; xxx implement layer-config
    (λ (w h dc)
      (define glctx (send dc get-gl-context))
      (unless glctx
        (error 'draw "Could not initialize OpenGL!"))
      (send glctx call-as-current
            (λ ()
              (unless (unbox draw-on-crt-b)
                (set-box! draw-on-crt-b (make-draw-on-crt width height)))
              (unless (unbox draw-sprites-b)
                (set-box!
                 draw-sprites-b
                 (make-draw
                  texture-atlas texture-index palette-atlas
                  width height)))
              (when last-sprites
                ((unbox draw-on-crt-b)
                 w h
                 (λ () ((unbox draw-sprites-b)
                        last-sprites))))
              (send glctx swap-buffers))))))

(define gui-mode 'gl-core)
(provide
 (contract-out
  [gui-mode symbol?]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
