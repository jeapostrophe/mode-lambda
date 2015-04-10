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

(define (pair->fpair w h)
  (f32vector (* 1.0 w) (* 1.0 h)))
(define (set-uniform-fpair! program-id uniform-name the-fpair)
  (glUniform2fv (glGetUniformLocation program-id uniform-name) 1 the-fpair))
(struct scale-info (logical scale scaled texture screen))
(define (set-uniform-scale-info! program-id si)
  (match-define (scale-info logical scale scaled texture screen) si)
  (set-uniform-fpair! program-id "LogicalSize" logical)
  (glUniform1f (glGetUniformLocation program-id "Scale") scale)
  (set-uniform-fpair! program-id "ScaledSize" scaled)
  (set-uniform-fpair! program-id "TextureSize" texture)
  (set-uniform-fpair! program-id "ScreenSize" screen))

(define (set-viewport/fpair! the-fp)
  (define width (inexact->exact (f32vector-ref the-fp 0)))
  (define height (inexact->exact (f32vector-ref the-fp 1)))
  (glViewport 0 0 width height))

(struct delayed-fbo (tex-count [texs #:mutable] [fbo #:mutable]))
(define (make-delayed-fbo tex-count)
  (delayed-fbo tex-count #f #f))
(define (initialize-dfbo! dfbo the-si)
  (define the-fp (scale-info-texture the-si))
  (define tex-width (inexact->exact (f32vector-ref the-fp 0)))
  (define tex-height (inexact->exact (f32vector-ref the-fp 1)))
  
  (match-define (delayed-fbo tex-count texs fbo) dfbo)
  (when fbo
    (glDeleteFramebuffers 1 (u32vector fbo)))
  (when texs
    (glDeleteTextures (length texs) (list->u32vector texs)))
  (define new-texs
    (for/list ([i (in-range tex-count)])
      (make-target-texture tex-width tex-height)))
  (set-delayed-fbo-texs! dfbo new-texs)
  (set-delayed-fbo-fbo! dfbo (make-fbo new-texs)))

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

  (define render-layers!
    (let ()
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

      (define layer-program (glCreateProgram))
      (bind-attribs/cstruct-info layer-program _sprite-data:info)

      (define-shader-source layer-vert "gl/ngl.vertex.glsl")
      (define-shader-source layer-fragment "gl/ngl.fragment.glsl")

      (compile-shader GL_VERTEX_SHADER layer-program layer-vert)
      (compile-shader GL_FRAGMENT_SHADER layer-program layer-fragment)

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
                     (gl-texture-index GL_TEXTURE3)))

      (define layer-dfbo (make-delayed-fbo LAYERS))

      (define DrawnMult 6)

      (define (make-sprite-draw!)
        (define layer-vao (glGen glGenVertexArrays))
        (define layer-vbo (glGen glGenBuffers))
        (nest
         ([with-vertexarray (layer-vao)]
          [with-arraybuffer (layer-vbo)])
         (define-attribs/cstruct-info _sprite-data:info))

        (define actual-update!
          (let ()
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

            (define SpriteData-count 0)
            (define *initialize-count* (* 2 512))
            (define SpriteData #f)

            (λ (objects)
              (define early-count (count-objects objects))
              (define SpriteData-count:new (max *initialize-count* early-count))
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
              early-count)))

        (define update!
          (let ()
            (define last-objects #f)
            (define last-count 0)
            (λ (objects)
              (cond
               [(eq? last-objects objects)
                last-count]
               [else
                (set! last-objects objects)
                (define early-count (actual-update! objects))
                (set! last-count early-count)
                early-count]))))

        (λ (objects)
          (define obj-count (update! objects))
          (nest
           ([with-vertexarray (layer-vao)]
            [with-vertex-attributes ((length _sprite-data:info))])
           (glDrawArrays GL_TRIANGLES 0 (* DrawnMult obj-count)))))

      (define draw-static! (make-sprite-draw!))
      (define draw-dynamic! (make-sprite-draw!))

      (λ (update-scale? the-scale-info static-st dynamic-st)
        (when update-scale?
          (initialize-dfbo! layer-dfbo the-scale-info))

        (nest
         ([with-framebuffer ((delayed-fbo-fbo layer-dfbo))]
          [with-texture (GL_TEXTURE0 SpriteAtlasId)]
          [with-texture (GL_TEXTURE1 PaletteAtlasId)]
          [with-texture (GL_TEXTURE2 SpriteIndexId)]
          [with-texture (GL_TEXTURE3 LayerConfigId)]
          [with-feature (GL_BLEND)]
          [with-program (layer-program)])
         (set-uniform-scale-info! layer-program the-scale-info)
         (glDrawBuffers LAYERS
                        (list->s32vector
                         (for/list ([i (in-range LAYERS)])
                           (GL_COLOR_ATTACHMENTi i))))
         (glClearColor 0.0 0.0 0.0 0.0)
         (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
         (glClear GL_COLOR_BUFFER_BIT)
         (set-viewport/fpair! (scale-info-texture the-scale-info))

         (draw-static! static-st)
         (draw-dynamic! dynamic-st))

        (delayed-fbo-texs layer-dfbo))))

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
                         (+ 1 i)))))

      (define combine-vao (glGen glGenVertexArrays))

      (define combine-dfbo (make-delayed-fbo 1))

      (λ (update-scale? the-scale-info LayerTargets)
        (when update-scale?
          (initialize-dfbo! combine-dfbo the-scale-info))

        (nest
         ([with-framebuffer ((delayed-fbo-fbo combine-dfbo))]
          [with-vertexarray (combine-vao)]
          [with-texture (GL_TEXTURE0 LayerConfigId)]
          [with-textures (1 LayerTargets)]
          [with-program (combine-program)])
         (set-uniform-scale-info! combine-program the-scale-info)
         (glClearColor 0.0 0.0 0.0 0.0)
         (glClear GL_COLOR_BUFFER_BIT)
         (set-viewport/fpair! (scale-info-texture the-scale-info))
         (glDrawArrays GL_TRIANGLES 0 FULLSCREEN_VERTS))

        (first (delayed-fbo-texs combine-dfbo)))))

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
        (glUniform1i (glGetUniformLocation screen-program "CombinedTex")
                     (gl-texture-index GL_TEXTURE0)))

      (define screen-vao (glGen glGenVertexArrays))

      (λ (update-scale? the-scale-info combine-tex)
        (nest
         ([with-program (screen-program)]
          [with-texture (GL_TEXTURE0 combine-tex)]
          [with-vertexarray (screen-vao)])
         (set-uniform-scale-info! screen-program the-scale-info)
         (glClearColor 0.0 0.0 0.0 0.0)
         (glClear GL_COLOR_BUFFER_BIT)
         (set-viewport/fpair! (scale-info-screen the-scale-info))
         (glDrawArrays GL_TRIANGLES 0 FULLSCREEN_VERTS)))))

  (define LogicalSize (pair->fpair width height))

  (define the-scale-info #f)
  (λ (screen-width screen-height layer-config static-st dynamic-st)
    (define scale
      (compute-nice-scale screen-width width screen-height height))
    (define update-scale?
      (not (and the-scale-info 
                (= (scale-info-scale the-scale-info)
                   scale))))
    (when update-scale?
      (define sca-width (* scale width))
      (define sca-height (* scale height))
      (define ScaledSize (pair->fpair sca-width sca-height))
      (define tex-width (inexact->exact (ceiling sca-width)))
      (define tex-height (inexact->exact (ceiling sca-height)))
      (define TextureSize (pair->fpair tex-width tex-height))
      (define ScreenSize (pair->fpair screen-width screen-height))
      (set! the-scale-info
            (scale-info LogicalSize scale ScaledSize TextureSize ScreenSize)))
    (update-layer-config! layer-config)
    (define LayerTargets
      (render-layers! update-scale? the-scale-info static-st dynamic-st))
    (define combine-tex
      (combine-layers! update-scale? the-scale-info LayerTargets))
    (draw-screen! update-scale? the-scale-info combine-tex)))

(define (stage-draw/dc csd width height)
  (define draw #f)
  (λ (layer-config static-st dynamic-st)
    (λ (w h dc)
      (local-require racket/class)
      (define glctx (send dc get-gl-context))
      (unless glctx
        (error 'draw "Could not initialize OpenGL!"))
      (send glctx call-as-current
            (λ ()
              (unless draw
                (set! draw (make-draw csd width height (gl-filter-mode))))
              (draw w h layer-config static-st dynamic-st)
              (send glctx swap-buffers))))))

(define gl-filter-mode (make-parameter 'std))

(define gui-mode 'gl-core)
(provide
 (contract-out
  [gl-filter-mode (parameter/c symbol?)]
  [gui-mode symbol?]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
