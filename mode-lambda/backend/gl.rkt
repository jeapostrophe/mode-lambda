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

(define (make-draw csd width height screen-mode)
  (eprintf "You are using OpenGL ~a\n" (gl-version))

  (match-define
   (compiled-sprite-db atlas-size atlas-bs spr->idx idx->w*h*tx*ty
                       pal-size pal-bs pal->idx)
   csd)

  (define LayerConfigId (make-2dtexture))
  (define update-layer-config!
    (let ()
      (define last-layer-config #f)
      
      (λ (layer-config)
        (unless (equal? layer-config last-layer-config)
          (set! last-layer-config layer-config)
          (with-texture (GL_TEXTURE3 LayerConfigId)
            (load-texture/float-bytes LAYER-VALUES LAYERS
                                      (layer-config->bytes layer-config)))))))

  (define LayerTargets
    (for/list ([i (in-range LAYERS)])
      (make-target-texture width height)))
  (define render-layers!
    (let ()
      (define layer-program (glCreateProgram))
      (bind-attribs/cstruct-info layer-program _sprite-data:info)

      (define-shader-source layer-vert "gl/ngl.vertex.glsl")
      (define-shader-source layer-fragment "gl/ngl.fragment.glsl")

      (compile-shader GL_VERTEX_SHADER layer-program layer-vert)
      (compile-shader GL_FRAGMENT_SHADER layer-program layer-fragment)

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

      (define layer-fbo (make-fbo LayerTargets))
      (for ([i (in-range LAYERS)])
        (glBindFragDataLocation layer-program i (format "out_Color~a" i)))

      (glLinkProgram&check layer-program)
      (with-program (layer-program)
        (glUniform1i (glGetUniformLocation layer-program "SpriteAtlasTex")
                     (gl-texture-index GL_TEXTURE0))
        (glUniform1i (glGetUniformLocation layer-program "PaletteAtlasTex")
                     (gl-texture-index GL_TEXTURE1))
        (glUniform1i (glGetUniformLocation layer-program "SpriteIndexTex")
                     (gl-texture-index GL_TEXTURE2))
        (glUniform1i (glGetUniformLocation layer-program "LayerConfigTex")
                     (gl-texture-index GL_TEXTURE3))
        (glUniform1ui (glGetUniformLocation layer-program "ViewportWidth") width)
        (glUniform1ui (glGetUniformLocation layer-program "ViewportHeight") height))

      (define layer-vao (glGen glGenVertexArrays))
      (define layer-vbo (glGen glGenBuffers))
      (nest
       ([with-vertexarray (layer-vao)]
        [with-arraybuffer (layer-vbo)])
       (define-attribs/cstruct-info _sprite-data:info))

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

      (λ (static-st dynamic-st)
        ;; xxx optimize static
        (define objects (cons static-st dynamic-st))
        (define early-count (count-objects objects))
        (define SpriteData-count:new (max *initialize-count* early-count))

        (nest
         ([with-framebuffer (layer-fbo)]
          [with-vertexarray (layer-vao)]
          [with-vertex-attributes ((length _sprite-data:info))]
          [with-texture (GL_TEXTURE0 SpriteAtlasId)]
          [with-texture (GL_TEXTURE1 PaletteAtlasId)]
          [with-texture (GL_TEXTURE2 SpriteIndexId)]
          [with-texture (GL_TEXTURE3 LayerConfigId)]
          [with-feature (GL_BLEND)]
          [with-program (layer-program)])
         (with-arraybuffer (layer-vbo)
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

         (glDrawBuffers LAYERS
                        (list->s32vector
                         (for/list ([i (in-range LAYERS)])
                           (GL_COLOR_ATTACHMENTi i))))
         (glClearColor 0.0 0.0 0.0 0.0)
         (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
         (glClear GL_COLOR_BUFFER_BIT)
         (glViewport 0 0 width height)
         (glDrawArrays GL_TRIANGLES 0 (* DrawnMult early-count))))))

  (define combine-tex (make-target-texture width height))
  (define combine-layers!
    (let ()
      (define combine-program (glCreateProgram))
      (define-shader-source combine-vert "gl/combine.vertex.glsl")
      (define-shader-source combine-fragment "gl/combine.fragment.glsl")
      (compile-shader GL_VERTEX_SHADER combine-program combine-vert)
      (compile-shader GL_FRAGMENT_SHADER combine-program combine-fragment)
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
      (define combine-fbo (make-fbo (list combine-tex)))

      (λ ()
        (nest
         ([with-framebuffer (combine-fbo)]
          [with-vertexarray (combine-vao)]
          [with-texture (GL_TEXTURE0 LayerConfigId)]
          [with-textures (1 LayerTargets)]
          [with-program (combine-program)])
         (glClearColor 0.0 0.0 0.0 0.0)
         (glClear GL_COLOR_BUFFER_BIT)
         (glViewport 0 0 width height)
         (glDrawArrays GL_TRIANGLES 0 FULLSCREEN_VERTS)))))

  (define draw-screen!
    (let ()
      (define screen-program (glCreateProgram))

      (define-shader-source crt-fragment "gl/crt.fragment.glsl")
      (define-shader-source crt-vert "gl/crt.vertex.glsl")
      (define-shader-source std-fragment "gl/std.fragment.glsl")
      (define-shader-source std-vert "gl/std.vertex.glsl")

      (define-values (screen-fragment screen-vert)
        (match screen-mode
          ['crt (values crt-fragment crt-vert)]
          ['std (values std-fragment std-vert)]))

      (compile-shader GL_FRAGMENT_SHADER screen-program screen-fragment)
      (compile-shader GL_VERTEX_SHADER screen-program screen-vert)

      (glLinkProgram&check screen-program)

      (with-program (screen-program)
        (glUniform1i (glGetUniformLocation screen-program "rubyTexture")
                     (gl-texture-index GL_TEXTURE0))
        (glUniform2fv (glGetUniformLocation screen-program "rubyInputSize")
                      1 (f32vector (* 1. width) (* 1. height))))

      (define screen-vao (glGen glGenVertexArrays))

      (λ (actual-screen-width actual-screen-height)
        (nest
         ([with-program (screen-program)]
          [with-texture (GL_TEXTURE0 combine-tex)]
          [with-vertexarray (screen-vao)])
         (glUniform2fv (glGetUniformLocation screen-program "rubyOutputSize") 1
                       (f32vector (* 1. actual-screen-width)
                                  (* 1. actual-screen-height)))
         (glClearColor 0.0 0.0 0.0 0.0)
         (glClear GL_COLOR_BUFFER_BIT)
         (glViewport 0 0 actual-screen-width actual-screen-height)
         (glDrawArrays GL_TRIANGLES 0 FULLSCREEN_VERTS)))))

  (λ (actual-screen-width actual-screen-height layer-config static-st dynamic-st)
    (update-layer-config! layer-config)
    (render-layers! static-st dynamic-st)
    (combine-layers!)
    (draw-screen! actual-screen-width actual-screen-height)))

(define (stage-draw/dc csd width height)
  (define draw
    (make-delayed-until-gl-is-around
     (λ () (make-draw csd width height 'crt))))
  (λ (layer-config static-st dynamic-st)
    (λ (w h dc)
      (local-require racket/class)
      (define glctx (send dc get-gl-context))
      (unless glctx
        (error 'draw "Could not initialize OpenGL!"))
      (send glctx call-as-current
            (λ ()
              ((draw) w h layer-config static-st dynamic-st)
              (send glctx swap-buffers))))))

(define gui-mode 'gl-core)
(provide
 (contract-out
  [gui-mode symbol?]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
