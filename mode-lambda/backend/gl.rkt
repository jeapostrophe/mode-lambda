#lang racket/base
(require ffi/cvector
         ffi/unsafe/cvector
         ffi/vector
         mode-lambda/backend/lib
         mode-lambda/core
         racket/contract
         racket/list
         racket/match
         web-server/templates
         opengl
         (only-in math/base
                  sum)
         (only-in ffi/unsafe
                  ctype-sizeof
                  ctype->layout
                  _float))

(define-syntax-rule (glsl-include p) (include-template p))

(define (make-delayed-until-gl-is-around t)
  (define c #f)
  (define r void)
  (λ ()
    (unless c
      (set! c #t)
      (set! r (t)))
    r))

(define (cvector-set*! vec k . vs)
  (for ([v (in-list vs)]
        [i (in-naturals)])
    (cvector-set! vec (+ k i) v)))

;; My GL lib

(define (make-2dtexture)
  (define Id (u32vector-ref (glGenTextures 1) 0))
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

(define-syntax-rule (define-with-state with-state (F static-arg ...))
  (define-syntax-rule (with-state (dyn-arg (... ...)) . body)
    (begin (F static-arg ... dyn-arg (... ...))
           (let () . body)
           (F static-arg ... 0))))

(define-with-state with-program (glUseProgram))
(define-with-state with-vertexarray (glBindVertexArray))
(define-syntax-rule (with-texture (GL_TEXTUREx TextureId) . body)
  (begin (glActiveTexture GL_TEXTUREx)
         (glBindTexture GL_TEXTURE_2D TextureId)
         (let () . body)
         (glActiveTexture GL_TEXTUREx)
         (glBindTexture GL_TEXTURE_2D 0)))
(define-with-state with-framebuffer (glBindFramebuffer GL_FRAMEBUFFER))
(define-with-state with-renderbuffer (glBindRenderbuffer GL_RENDERBUFFER))
(define-with-state with-arraybuffer (glBindBuffer GL_ARRAY_BUFFER))

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
    ((if int? glVertexAttribIPointer* glVertexAttribPointer)
     Index HowMany type
     #f
     (ctype-sizeof _sprite-data)
     byte-offset)
    (glEnableVertexAttribArray Index)))

(define-syntax-rule
  (define-vertex-attrib-array*
    [AttribId AttribStart AttribEnd] ...)
  (begin
    (define-vertex-attrib-array AttribId AttribStart AttribEnd)
    ...))

;; Old Code from GB (gl-util.rkt)

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

;; Old Code from GB (crt.rkt)

(define (make-draw-on-crt crt-width crt-height mode)
  (eprintf "You are using OpenGL ~a\n" (gl-version))

  (define myTexture (u32vector-ref (glGenTextures 1) 0))
  (with-texture (GL_TEXTURE0 myTexture)
    (2D-defaults)
    (glTexImage2D
     GL_TEXTURE_2D 0 GL_RGBA8 crt-width crt-height 0
     GL_RGBA GL_UNSIGNED_BYTE
     0))

  (define myRB (u32vector-ref (glGenRenderbuffers 1) 0))
  (with-renderbuffer (myRB)
    (glRenderbufferStorage GL_RENDERBUFFER
                           GL_DEPTH_COMPONENT24
                           crt-width crt-height))

  (define myFBO (u32vector-ref (glGenFramebuffers 1) 0))
  (with-framebuffer (myFBO)
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
       (exit 1)]))

  (define shader_program (glCreateProgram))

  (define EFFECT_VERTS 6)

  (define-shader-source crt-fragment "gl/crt.fragment.glsl")
  (define-shader-source crt-vert "gl/crt.vertex.glsl")
  (define-shader-source std-fragment "gl/std.fragment.glsl")
  (define-shader-source std-vert "gl/std.vertex.glsl")

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

  (with-program (shader_program)
    (glUniform1i (glGetUniformLocation shader_program "rubyTexture")
                 (gl-texture-index GL_TEXTURE0))
    (glUniform2fv (glGetUniformLocation shader_program "rubyInputSize")
                  1 (f32vector (* 1. crt-width) (* 1. crt-height))))

  (define vao (u32vector-ref (glGenVertexArrays 1) 0))

  (define (new-draw-on-crt actual-screen-width actual-screen-height do-the-drawing)
    (with-framebuffer (myFBO)
      (glViewport 0 0 crt-width crt-height)
      (do-the-drawing))

    (with-program (shader_program)
      (glUniform2fv (glGetUniformLocation shader_program "rubyOutputSize") 1
                    (f32vector (* 1. actual-screen-width) (* 1. actual-screen-height)))
      (glClearColor 0.0 0.0 0.0 0.0)
      (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (glViewport 0 0 actual-screen-width actual-screen-height)
      (with-texture (GL_TEXTURE0 myTexture)
        (with-vertexarray (vao)
          (glDrawArrays GL_TRIANGLES 0 EFFECT_VERTS)))))

  new-draw-on-crt)

;; Old Code from GB (ngl.rkt)

(define num->pow2 integer-length)

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

  (define AttributeCount 5)

  (define ProgramId (glCreateProgram))
  (match-define (list in_DX_DY-attrib in_MX_MY_THETA_A-attrib
                      in_SPR_PAL-attrib in_LAYER_R_G_B-attrib
                      in_HORIZ_VERT-attrib)
                (build-list AttributeCount (λ (x) x)))
  (glBindAttribLocation ProgramId         in_DX_DY-attrib "in_DX_DY")
  (glBindAttribLocation ProgramId in_MX_MY_THETA_A-attrib "in_MX_MY_THETA_A")
  (glBindAttribLocation ProgramId       in_SPR_PAL-attrib "in_SPR_PAL")
  (glBindAttribLocation ProgramId   in_LAYER_R_G_B-attrib "in_LAYER_R_G_B")
  (glBindAttribLocation ProgramId    in_HORIZ_VERT-attrib "in_HORIZ_VERT")

  (define-shader-source ngl-vert "gl/ngl.vertex.glsl")
  (define-shader-source ngl-fragment "gl/ngl.fragment.glsl")

  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId ngl-vert)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId ngl-fragment)

  (define *initialize-count* (* 2 512))

  (define (install-objects! t)
    (tree-fold (λ (offset o)
                 (install-object! offset o)
                 (add1 offset))
               0 t))
  (define (count-objects t)
    (tree-fold (λ (count o) (add1 count)) 0 t))

  (define SpriteAtlasId (make-2dtexture))
  (with-texture (GL_TEXTURE0 SpriteAtlasId)
    (load-texture/bytes atlas-size atlas-size atlas-bs))
  (define PaletteAtlasId (make-2dtexture))
  (with-texture (GL_TEXTURE0 PaletteAtlasId)
    (load-texture/bytes PALETTE-DEPTH pal-size pal-bs))
  (define SpriteIndexId (make-2dtexture))
  (with-texture (GL_TEXTURE0 SpriteIndexId)
    (define sprite-index-count
      (vector-length idx->w*h*tx*ty))

    (define index-values 4)
    (define index-bytes-per-value (ctype-sizeof _float))
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

    (load-texture/float-bytes
     index-values effective-sprite-index-count index-bin))

  (define LayerConfigId (make-2dtexture))

  (define LayersId (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D_ARRAY LayersId)
  (glTexStorage3D GL_TEXTURE_2D_ARRAY 1 GL_RGBA8 width height LAYERS)
  (glBindTexture GL_TEXTURE_2D_ARRAY 0)

  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)

  (with-program (ProgramId)
    (glUniform1i (glGetUniformLocation ProgramId "SpriteAtlasTex")
                 (gl-texture-index GL_TEXTURE0))
    (glUniform1i (glGetUniformLocation ProgramId "PaletteAtlasTex")
                 (gl-texture-index GL_TEXTURE1))
    (glUniform1i (glGetUniformLocation ProgramId "SpriteIndexTex")
                 (gl-texture-index GL_TEXTURE2))
    (glUniform1i (glGetUniformLocation ProgramId "LayerConfigTex")
                 (gl-texture-index GL_TEXTURE3))
    (glUniform1ui (glGetUniformLocation ProgramId "ViewportWidth") width)
    (glUniform1ui (glGetUniformLocation ProgramId "ViewportHeight") height))

  (define VaoId (u32vector-ref (glGenVertexArrays 1) 0))
  (define VboId (u32vector-ref (glGenBuffers 1) 0))
  (with-vertexarray (VaoId)    
    (with-arraybuffer (VboId)
      (define-vertex-attrib-array*
        [        in_DX_DY-attrib  0  1]
        [in_MX_MY_THETA_A-attrib  2  5]
        [      in_SPR_PAL-attrib  6  7]
        [  in_LAYER_R_G_B-attrib  8 11]
        [   in_HORIZ_VERT-attrib 12 13])))

  (define last-layer-config #f)

  (define (draw layer-config static-st dynamic-st)
    (unless (equal? layer-config last-layer-config)
      (set! last-layer-config layer-config)
      (define lc-values 12)
      (define lc-bytes-per-value (ctype-sizeof _float))
      (define lc-bs (make-bytes (* lc-values LAYERS lc-bytes-per-value)))
      (for ([i (in-naturals)]
            [lc (in-vector layer-config)])
        (match-define
         (layer-data Lcx Lcy Lhw Lhh Lmx Lmy Ltheta
                     mode7-coeff horizon fov wrap-x? wrap-y?)
         (or lc default-layer))
        (for ([o (in-naturals)]
              [v (in-list (list Lcx Lcy Lhw Lhh Lmx Lmy Ltheta
                                mode7-coeff horizon fov
                                (if wrap-x? 1.0 0.0)
                                (if wrap-y? 1.0 0.0)))])
          (real->floating-point-bytes
           v lc-bytes-per-value
           (system-big-endian?) lc-bs
           (+ (* lc-values lc-bytes-per-value i)
              (* lc-bytes-per-value o)))))

      (with-texture (GL_TEXTURE3 LayerConfigId)
        (load-texture/float-bytes lc-values LAYERS lc-bs)))

    ;; xxx optimize static
    (define objects (cons static-st dynamic-st))

    (with-vertexarray (VaoId)
      (for ([i (in-range AttributeCount)])
        (glEnableVertexAttribArray i))

      ;; xxx use scheme/nest
      (with-texture (GL_TEXTURE0 SpriteAtlasId)
        (with-texture (GL_TEXTURE1 PaletteAtlasId)
          (with-texture (GL_TEXTURE2 SpriteIndexId)
            (with-texture (GL_TEXTURE3 LayerConfigId)
              (with-arraybuffer (VboId)
                (define early-count (count-objects objects))
                (define SpriteData-count:new (max *initialize-count* early-count))

                (unless (>= SpriteData-count SpriteData-count:new)
                  (define SpriteData-count:old SpriteData-count)
                  (set! SpriteData-count
                        (max (* 2 SpriteData-count)
                             SpriteData-count:new))
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
                (glUnmapBuffer GL_ARRAY_BUFFER))

              (with-program (ProgramId)
                (glEnable GL_DEPTH_TEST)
                (glClearColor 0.0 0.0 0.0 1.0)

                (glEnable GL_BLEND)
                ;; xxx blending is still wrong (but may not matter when i implement layers myself)
                (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

                (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

                ;; xxx draw this onto a 1xLAYERS 2D texture array
                (define drawn-count this-count)
                (glDrawArrays
                 GL_TRIANGLES 0
                 (* DrawnMult drawn-count))

                ;; xxx do the layer combination pass

                (glDisable GL_DEPTH_TEST)
                (glDisable GL_BLEND)

                (for ([i (in-range AttributeCount)])
                  (glDisableVertexAttribArray i)))))))))

  draw)

;; New interface

(define (stage-draw/dc csd width height)
  (define draw-on-crt
    (make-delayed-until-gl-is-around
     (λ ()
       (make-draw-on-crt width height 'crt))))
  (define draw-sprites
    (make-delayed-until-gl-is-around
     (λ ()
       (make-draw csd width height))))
  (λ (layer-config static-st dynamic-st)
    (λ (w h dc)
      (local-require racket/class)
      (define glctx (send dc get-gl-context))
      (unless glctx
        (error 'draw "Could not initialize OpenGL!"))
      (send glctx call-as-current
            (λ ()
              ((draw-on-crt) w h
               (λ () ((draw-sprites) layer-config static-st dynamic-st)))
              (send glctx swap-buffers))))))

(define gui-mode 'gl-core)
(provide
 (contract-out
  [gui-mode symbol?]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
