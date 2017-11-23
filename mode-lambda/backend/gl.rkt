#lang racket/base
(require ffi/cvector
         ffi/unsafe/cvector
         (only-in ffi/vector
                  u32vector
                  list->s32vector
                  s32vector-ref)
         mode-lambda/backend/gl/util
         mode-lambda/backend/lib
         mode-lambda/core
         mode-lambda/sprite-index
         racket/contract
         racket/list
         racket/match
         web-server/templates
         opengl
         scheme/nest
         racket/require
         (for-syntax racket/base)
         racket/flonum
         racket/fixnum
         (only-in ffi/unsafe
                  ctype-sizeof
                  _float))

(define QUAD_VERTS 4)

(define LAYER-VALUES 12)
(define (layer-config->bytes how-many-layers layer-config)
  (define lc-bytes-per-value (ctype-sizeof _float))
  (define lc-bs (make-bytes (* LAYER-VALUES how-many-layers lc-bytes-per-value)))
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

(define VERTEX_SPEC_L
  (for*/list ([xc '(-1 0 +1)]
              [yc '(-1 0 +1)])
    (list "ivec2(" xc "," yc ")")))
(define INSTANCES_PER_SPR
  (length VERTEX_SPEC_L))
(define VERTEX_SPEC
  (add-between VERTEX_SPEC_L ","))

(define (make-draw csd width.fx height.fx how-many-layers screen-mode)
  (define width (fx->fl width.fx))
  (define height (fx->fl height.fx))
  (eprintf "You are using OpenGL ~v with gl-backend-version of ~v\n"
           (gl-version)
           (gl-backend-version))

  (define shot! (gl-screenshot!))

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
            (load-texture/float-bytes LAYER-VALUES how-many-layers
                                      (layer-config->bytes how-many-layers
                                                           layer-config)))))))

  (define render-layers!
    (let ()
      ;; xxx allow these to be updated
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

      (define tmp-vao (glGen glGenVertexArrays))
      (with-vertexarray (tmp-vao)
        (glLinkProgram& layer-program 'layer)
        (with-program (layer-program)
          (glUniform1i (glGetUniformLocation layer-program "SpriteAtlasTex")
                       (gl-texture-index GL_TEXTURE0))
          (glUniform1i (glGetUniformLocation layer-program "PaletteAtlasTex")
                       (gl-texture-index GL_TEXTURE1))
          (glUniform1i (glGetUniformLocation layer-program "SpriteIndexTex")
                       (gl-texture-index GL_TEXTURE2))
          (glUniform1i (glGetUniformLocation layer-program "LayerConfigTex")
                       (gl-texture-index GL_TEXTURE3))
          (glValidateProgram& layer-program 'layer)))
      (glDeleteVertexArrays 1 (u32vector tmp-vao))

      (define layer-dfbos
        (for/list ([i (in-range 2)])
          (make-delayed-fbo how-many-layers)))

      (define (make-sprite-draw!)
        (define layer-vao (glGen glGenVertexArrays))
        (define layer-vbo (glGen glGenBuffers))
        (nest
            ([with-vertexarray (layer-vao)]
             [with-arraybuffer (layer-vbo)])
          (define-attribs/cstruct-info INSTANCES_PER_SPR _sprite-data:info))

        (define actual-update!
          (make-update-vbo-buffer-with-objects! _sprite-data layer-vbo))

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
                 (define early-count
                   (actual-update! objects))
                 (set! last-count early-count)
                 early-count]))))

        (λ (objects)
          (define obj-count (update! objects))
          (nest
              ([with-vertexarray (layer-vao)]
               [with-vertex-attributes ((length _sprite-data:info))])
            (glDrawArraysInstanced GL_TRIANGLE_STRIP 0
                                   QUAD_VERTS
                                   (fx* INSTANCES_PER_SPR obj-count)))))

      (define draw-static! (make-sprite-draw!))
      (define draw-dynamic! (make-sprite-draw!))
      (define front? #f)

      (λ (update-scale? the-scale-info static-st dynamic-st)
        (when update-scale?
          (for ([layer-dfbo (in-list layer-dfbos)])
            (initialize-dfbo! layer-dfbo the-scale-info)))

        (define layer-dfbo
          (list-ref layer-dfbos
                    (if front? 0 1)))
        (set! front? (not front?))

        (for ([active-layeri (in-range how-many-layers)])
          (nest
              ([with-framebuffer ((delayed-fbo-fbo layer-dfbo active-layeri))]
               [with-texture (GL_TEXTURE0 SpriteAtlasId)]
               [with-texture (GL_TEXTURE1 PaletteAtlasId)]
               [with-texture (GL_TEXTURE2 SpriteIndexId)]
               [with-texture (GL_TEXTURE3 LayerConfigId)]
               [with-feature (GL_BLEND)]
               [with-program (layer-program)])
            (set-uniform-scale-info! layer-program the-scale-info)
            (glUniform1f (glGetUniformLocation layer-program "ActiveLayer")
                         (real->double-flonum active-layeri))
            (glClearColor 0.0 0.0 0.0 0.0)
            (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
            (glClear GL_COLOR_BUFFER_BIT)
            (set-viewport/fpair! (scale-info-texture the-scale-info))

            (draw-static! static-st)
            (draw-dynamic! dynamic-st)))

        (delayed-fbo-tex layer-dfbo))))

  (define combine-layers!
    (let ()
      (define combine-vao (glGen glGenVertexArrays))

      (define combine-program (glCreateProgram))
      (define-shader-source combine-vert "gl/combine.vertex.glsl")
      (define-shader-source combine-fragment "gl/combine.fragment.glsl")
      (compile-shader GL_VERTEX_SHADER combine-program combine-vert)
      (compile-shader GL_FRAGMENT_SHADER combine-program combine-fragment)
      (with-vertexarray (combine-vao)
        (glLinkProgram& combine-program 'combine)
        (with-program (combine-program)
          (glUniform1i (glGetUniformLocation combine-program "LayerConfigTex")
                       (gl-texture-index GL_TEXTURE0))
          (glUniform1i (glGetUniformLocation combine-program "LayerTargets")
                       (gl-texture-index GL_TEXTURE1))
          (glValidateProgram& combine-program 'combine)))

      (define combine-dfbos
        (for/list ([i (in-range 2)])
          (make-delayed-fbo 1)))
      (define front? #f)

      (λ (update-scale? the-scale-info LayerTargetsTex)
        (when update-scale?
          (for ([combine-dfbo (in-list combine-dfbos)])
            (initialize-dfbo! combine-dfbo the-scale-info)))

        (define combine-dfbo
          (list-ref combine-dfbos (if front? 0 1)))
        (set! front? (not front?))

        (nest
            ([with-framebuffer ((delayed-fbo-fbo combine-dfbo 0))]
             [with-vertexarray (combine-vao)]
             [with-texture (GL_TEXTURE0 LayerConfigId)]
             [with-texture-array (GL_TEXTURE1 LayerTargetsTex)]
             [with-program (combine-program)])
          (set-uniform-scale-info! combine-program the-scale-info)
          (glClearColor 0.0 0.0 0.0 0.0)
          (glClear GL_COLOR_BUFFER_BIT)
          (set-viewport/fpair! (scale-info-texture the-scale-info))
          (glDrawArrays GL_TRIANGLE_STRIP 0 QUAD_VERTS))

        (delayed-fbo-tex combine-dfbo))))

  (define draw-screen!
    (let ()
      (define screen-vao (glGen glGenVertexArrays))

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

      (with-vertexarray (screen-vao)
        (glLinkProgram& screen-program 'screen)
        (with-program (screen-program)
          (glUniform1i (glGetUniformLocation screen-program "CombinedTex")
                       (gl-texture-index GL_TEXTURE0))
          (glValidateProgram& screen-program 'screen)))

      (λ (update-scale? the-scale-info combine-tex)
        (nest
            ([with-program (screen-program)]
             [with-texture (GL_TEXTURE0 combine-tex)]
             [with-vertexarray (screen-vao)])
          (set-uniform-scale-info! screen-program the-scale-info)
          (glClearColor 0.0 0.0 0.0 0.0)
          (glClear GL_COLOR_BUFFER_BIT)
          (set-viewport/fpair! (scale-info-screen the-scale-info))
          (glDrawArrays GL_TRIANGLE_STRIP 0 QUAD_VERTS)))))

  (define LogicalSize (pair->fpair width height))

  (define the-scale-info #f)
  (λ (screen-width.fx screen-height.fx layer-config static-st dynamic-st)
    (define screen-width (fx->fl screen-width.fx))
    (define screen-height (fx->fl screen-height.fx))
    ;; If this were 8/7, then we'd have the same PAR as the NES on a
    ;; CRT and thus get non-square pixels. The problem with this is
    ;; that I get non-uniform pixel sizes as we go across the screen,
    ;; so it looks really bad. So for now I'll leave it at 1.0, but I
    ;; have the dormant code here to come back to it.
    ;;
    ;; What I'd really like is a screen so big that I can draw each
    ;; pixels as an 8x7 rectangle. On a 1080 screen, that's 240x154,
    ;; which is too small. A UHD screen would give 480x308, which
    ;; would be big enough for a full NES screen. Of course, drawing
    ;; that way would have to be done differently, including
    ;; differently specifying the center of sprites, which would
    ;; stink.
    (define CRT-PIXEL-ASPECT-RATIO? #f)
    (define pixel-aspect-ratio
      (if CRT-PIXEL-ASPECT-RATIO?
        (fl/ 8.0 7.0)
        1.0))
    (define scale
      (compute-nice-scale pixel-aspect-ratio
                          screen-width.fx width.fx
                          screen-height.fx height.fx))
    (define update-scale?
      (not (and the-scale-info
                (fl= (scale-info-y-scale the-scale-info)
                     scale))))
    (when update-scale?
      (define x-scale (fl* pixel-aspect-ratio scale))
      (define y-scale scale)
      (define sca-width (fl* x-scale width))
      (define sca-height (fl* y-scale height))
      (define ScaledSize (pair->fpair sca-width sca-height))
      (define tex-width (flceiling sca-width))
      (define tex-height (flceiling sca-height))
      (define TextureSize (pair->fpair tex-width tex-height))
      (define ScreenSize (pair->fpair screen-width screen-height))
      (set! the-scale-info
            (scale-info LogicalSize x-scale y-scale ScaledSize TextureSize ScreenSize))
      (eprintf "~v\n" (vector (vector width height)
                              (vector x-scale y-scale)
                              (vector sca-width sca-height)
                              (vector tex-width tex-height)
                              (vector screen-width screen-height))))
    (update-layer-config! layer-config)
    (define LayerTargetsTex
      (render-layers! update-scale? the-scale-info static-st dynamic-st))
    (define combine-tex
      (combine-layers! update-scale? the-scale-info LayerTargetsTex))
    (when shot!
      (local-require ffi/vector
                     racket/file)
      (define the-fp (scale-info-texture the-scale-info))
      (define w (fl->fx (f32vector-ref the-fp 0)))
      (define h (fl->fx (f32vector-ref the-fp 1)))
      (define bs (make-bytes (fx* 4 (fx* w h))))
      (for ([i (in-naturals)]
            ;; XXX Figure out how to screenshot from texture array
            [t (in-list (cons combine-tex #;LayerTargets))])
        (with-texture (GL_TEXTURE0 t)
          (glGetTexImage GL_TEXTURE_2D 0 GL_RGBA GL_UNSIGNED_BYTE bs))
        (rgba->argb! bs)
        (shot! i w h bs)))
    (draw-screen! update-scale? the-scale-info combine-tex)))

(define-make-delayed-render
  stage-draw/dc
  make-draw
  (csd width height how-many-layers)
  ((gl-filter-mode))
  (layer-config static-st dynamic-st))

(define gl-filter-mode (make-parameter 'std))
(define gl-screenshot! (make-parameter #f))

(define gui-mode 'gl-core)
(provide
 (contract-out
  [gl-backend-version (parameter/c (apply or/c valid-gl-backends))]
  [gl-filter-mode (parameter/c symbol?)]
  [gl-screenshot! (parameter/c (-> exact-nonnegative-integer?
                                   exact-nonnegative-integer?
                                   exact-nonnegative-integer?
                                   bytes?
                                   void?))]
  [gui-mode symbol?]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
