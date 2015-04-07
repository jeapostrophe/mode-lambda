#lang racket/base
(require ffi/cvector
         ffi/unsafe/cvector
         ffi/vector
         mode-lambda/backend/gl/util
         mode-lambda/backend/lib
         mode-lambda/core
         racket/contract
         racket/list
         racket/match
         web-server/templates
         opengl
         scheme/nest
         (only-in math/base
                  sum)
         (only-in ffi/unsafe
                  ctype-sizeof
                  _float))

(define-syntax-rule (glsl-include p) (include-template p))

(define FULLSCREEN_VERTS 6)

(define (make-draw-screen crt-width crt-height mode)
  (eprintf "You are using OpenGL ~a\n" (gl-version))

  (define myTexture (make-target-texture crt-width crt-height))

  (define myRB (glGen glGenRenderbuffers))
  (with-renderbuffer (myRB)
    (glRenderbufferStorage GL_RENDERBUFFER
                           GL_DEPTH_COMPONENT24
                           crt-width crt-height))

  (define myFBO (glGen glGenFramebuffers))
  (with-framebuffer (myFBO)
    (glFramebufferTexture2D GL_DRAW_FRAMEBUFFER
                            (GL_COLOR_ATTACHMENTi 0)
                            GL_TEXTURE_2D myTexture 0)
    (glFramebufferRenderbuffer GL_FRAMEBUFFER
                               GL_DEPTH_ATTACHMENT
                               GL_RENDERBUFFER myRB))

  (define shader_program (glCreateProgram))

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

  (glLinkProgram&check shader_program)

  (with-program (shader_program)
    (glUniform1i (glGetUniformLocation shader_program "rubyTexture")
                 (gl-texture-index GL_TEXTURE0))
    (glUniform2fv (glGetUniformLocation shader_program "rubyInputSize")
                  1 (f32vector (* 1. crt-width) (* 1. crt-height))))

  (define vao (glGen glGenVertexArrays))

  (λ (actual-screen-width actual-screen-height do-the-drawing)
    (with-framebuffer (myFBO)
      (glViewport 0 0 crt-width crt-height)
      (do-the-drawing))

    (nest
     ([with-program (shader_program)]
      [with-texture (GL_TEXTURE0 myTexture)]
      [with-vertexarray (vao)])
     (glUniform2fv (glGetUniformLocation shader_program "rubyOutputSize") 1
                   (f32vector (* 1. actual-screen-width) (* 1. actual-screen-height)))
     (glClearColor 0.0 0.0 0.0 1.0)
     (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
     (glViewport 0 0 actual-screen-width actual-screen-height)
     (glDrawArrays GL_TRIANGLES 0 FULLSCREEN_VERTS))))

(define LAYER-VALUES 12)
(define (layer-config->bytes layer-config)
  (define lc-bytes-per-value (ctype-sizeof _float))
  (define lc-bs (make-bytes (* LAYER-VALUES LAYERS lc-bytes-per-value)))
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
       (+ (* LAYER-VALUES lc-bytes-per-value i)
          (* lc-bytes-per-value o)))))
  lc-bs)

(define INDEX-VALUES 4)
(define (sprite-index->bytes idx->w*h*tx*ty)
  (define sprite-index-count
    (vector-length idx->w*h*tx*ty))
  (define index-bytes-per-value (ctype-sizeof _float))
  (define index-bin
    (make-bytes (* INDEX-VALUES index-bytes-per-value
                   sprite-index-count)))
  (for ([vec (in-vector idx->w*h*tx*ty)]
        [i (in-naturals)])
    (for ([v (in-vector vec)]
          [o (in-naturals)])
      (real->floating-point-bytes
       v index-bytes-per-value
       (system-big-endian?) index-bin
       (+ (* INDEX-VALUES
             index-bytes-per-value
             i)
          (* index-bytes-per-value o)))))
  index-bin)

(define (count-objects t)
  (tree-fold (λ (count o) (add1 count)) 0 t))

(define (make-draw csd width height)
  (match-define
   (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                       pal-size pal-bs pal->idx)
   csd)
  
  ;; Main rendering step
  (define ProgramId (glCreateProgram))
  (bind-attribs/cstruct-info ProgramId _sprite-data:info)

  (define-shader-source ngl-vert "gl/ngl.vertex.glsl")
  (define-shader-source ngl-fragment "gl/ngl.fragment.glsl")

  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId ngl-vert)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId ngl-fragment)

  (define SpriteAtlasId (make-2dtexture))
  (with-texture (GL_TEXTURE0 SpriteAtlasId)
    (load-texture/bytes atlas-size atlas-size atlas-bs))
  (define PaletteAtlasId (make-2dtexture))
  (with-texture (GL_TEXTURE0 PaletteAtlasId)
    (load-texture/bytes PALETTE-DEPTH pal-size pal-bs))
  (define SpriteIndexId (make-2dtexture))
  (with-texture (GL_TEXTURE0 SpriteIndexId)
    (load-texture/float-bytes
     INDEX-VALUES (vector-length idx->w*h*tx*ty)
     (sprite-index->bytes idx->w*h*tx*ty)))

  (define LayerConfigId (make-2dtexture))
  (define LayerTargets
    (for/list ([i (in-range LAYERS)])
      (make-target-texture width height)))
  (define LayersRB (glGen glGenRenderbuffers))
  (with-renderbuffer (LayersRB)
    (glRenderbufferStorage GL_RENDERBUFFER
                           GL_DEPTH_COMPONENT24
                           width height))
  (define LayersFBO (glGen glGenFramebuffers))
  (with-framebuffer (LayersFBO)
    (for ([i (in-naturals)]
          [tex (in-list LayerTargets)])
      (glFramebufferTexture2D GL_DRAW_FRAMEBUFFER
                              (GL_COLOR_ATTACHMENTi i)
                              GL_TEXTURE_2D tex 0))
    (glFramebufferRenderbuffer GL_FRAMEBUFFER
                               GL_DEPTH_ATTACHMENT
                               GL_RENDERBUFFER LayersRB))
  (for ([i (in-range LAYERS)])
    (glBindFragDataLocation ProgramId i (format "out_Color~a" i)))
  (glLinkProgram&check ProgramId)
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
  (define VaoId (glGen glGenVertexArrays))
  (define VboId (glGen glGenBuffers))
  (nest
   ([with-vertexarray (VaoId)]
    [with-arraybuffer (VboId)])
   (define-attribs/cstruct-info _sprite-data:info))
  
  ;; Combining Step
  (define combine-program (glCreateProgram))
  (define-shader-source combine-vert "gl/combine.vertex.glsl")
  (define-shader-source combine-fragment "gl/combine.fragment.glsl")
  (define&compile-shader combine-vert-shader GL_VERTEX_SHADER
    combine-program combine-vert)
  (define&compile-shader combine-frag-shader GL_FRAGMENT_SHADER
    combine-program combine-fragment)
  (glLinkProgram&check combine-program)
  (with-program (combine-program)
    (glUniform1i (glGetUniformLocation combine-program "LayerConfigTex")
                 (gl-texture-index GL_TEXTURE0))
    (glUniform1iv (glGetUniformLocation combine-program "LayerTargets")
                  LAYERS
                  (list->s32vector
                   (for/list ([i (in-range LAYERS)])
                     (+ 1 i))))
    (glUniform1ui (glGetUniformLocation combine-program "ViewportWidth") width)
    (glUniform1ui (glGetUniformLocation combine-program "ViewportHeight") height))
  (define combine-vao (glGen glGenVertexArrays))

  (define SpriteData-count 0)
  (define *initialize-count* (* 2 512))
  (define SpriteData #f)
  (define DrawnMult 6)

  (define (install-object! i o)
    (define-syntax-rule (point-install! Horiz Vert j ...)
      (begin
        (set-sprite-data-horiz! o Horiz)
        (set-sprite-data-vert! o Vert)
        (cvector-set! SpriteData (+ (* i DrawnMult) j) o)
        ...))
    (point-install! -1 +1 0)
    (point-install! +1 +1 1 4)
    (point-install! -1 -1 2 3)
    (point-install! +1 -1 5))

  (define (install-objects! t)
    (tree-fold (λ (offset o)
                 (install-object! offset o)
                 (add1 offset))
               0 t))
  
  (define last-layer-config #f)

  (λ (layer-config static-st dynamic-st)
    (unless (equal? layer-config last-layer-config)
      (set! last-layer-config layer-config)
      (with-texture (GL_TEXTURE3 LayerConfigId)
        (load-texture/float-bytes LAYER-VALUES LAYERS
                                  (layer-config->bytes layer-config))))

    ;; xxx optimize static
    (define objects (cons static-st dynamic-st))
    (define early-count (count-objects objects))
    (define SpriteData-count:new (max *initialize-count* early-count))

    (nest
     ([with-framebuffer (LayersFBO)]
      [with-vertexarray (VaoId)]
      [with-vertex-attributes ((length _sprite-data:info))]
      [with-texture (GL_TEXTURE0 SpriteAtlasId)]
      [with-texture (GL_TEXTURE1 PaletteAtlasId)]
      [with-texture (GL_TEXTURE2 SpriteIndexId)]
      [with-texture (GL_TEXTURE3 LayerConfigId)]
      [with-feature (GL_BLEND)]
      [with-program (ProgramId)])
     (with-arraybuffer (VboId)
       (unless (>= SpriteData-count SpriteData-count:new)
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

       (install-objects! objects)
       (glUnmapBuffer GL_ARRAY_BUFFER))
     
     (glDrawBuffers LAYERS (list->s32vector (for/list ([i (in-range LAYERS)]) (GL_COLOR_ATTACHMENTi i))))
     (glClearColor 0.0 0.0 0.0 0.0)
     (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
     (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))
     (glViewport 0 0 width height)
     (glDrawArrays GL_TRIANGLES 0 (* DrawnMult early-count)))
    
    (nest
     ([with-vertexarray (combine-vao)]
      [with-texture (GL_TEXTURE0 LayerConfigId)]
      [with-textures (1 LayerTargets)]
      [with-program (combine-program)])
     (glClearColor 0.0 0.0 0.0 0.0)
     (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
     (glViewport 0 0 width height)
     (glDrawArrays GL_TRIANGLES 0 FULLSCREEN_VERTS))))

(define (stage-draw/dc csd width height)
  (define draw-screen
    (make-delayed-until-gl-is-around
     (λ () (make-draw-screen width height 'crt))))
  (define draw-sprites
    (make-delayed-until-gl-is-around
     (λ () (make-draw csd width height))))
  (λ (layer-config static-st dynamic-st)
    (define draw-things
      (λ () ((draw-sprites) layer-config static-st dynamic-st)))
    (λ (w h dc)
      (local-require racket/class)
      (define glctx (send dc get-gl-context))
      (unless glctx
        (error 'draw "Could not initialize OpenGL!"))
      (send glctx call-as-current
            (λ ()
              ((draw-screen) w h draw-things)
              (send glctx swap-buffers))))))

(define gui-mode 'gl-core)
(provide
 (contract-out
  [gui-mode symbol?]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
