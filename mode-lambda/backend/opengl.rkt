#lang racket/base
(require ffi/vector
         ffi/cvector
         ffi/unsafe/cvector
         ffi/unsafe
         racket/file
         racket/runtime-path
         racket/match
         racket/fixnum
         racket/flonum
         (prefix-in ctc: racket/contract/base)
         opengl
         opengl/shader
         mode-lambda/core
         mode-lambda/backend/lib)
(module+ test
  (require rackunit))
;; xxx convert to fixnum/flonum

(define-syntax-rule (define-shader-source id path)
  (begin (define-runtime-path id-path path)
         (define id (file->string id-path))))

(define (draw-gl/dc draw!)
  (local-require racket/class)
  (λ (w h dc)
    (define glctx (send dc get-gl-context))
    (unless glctx
      (error 'draw-gl/dc "Could not initialize OpenGL!"))
    (send glctx call-as-current
          (λ ()
            (draw! w h)
            (send glctx swap-buffers)))))

;; xxx maybe use this in software too
;;
;; We want to find how to scale the CRT to the real screen, but it is
;; important to only use powers of two in the decimals and only up to
;; 2^5
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
(module+ test
  (define-syntax-rule (check-1q name x y e-r)
    (begin
      (define a-r (quotient* x y))
      (check-= a-r e-r 0
               (format "~a: ~a vs ~a"
                       name
                       (exact->inexact a-r)
                       (exact->inexact e-r)))))
  (define crt-width (* 32 16))
  (define crt-height (* 32 9))
  (define-syntax-rule (check-q* name (w h) (e-ws e-hs))
    (begin
      (check-1q (format "~a width(~a)" name w) w crt-width e-ws)
      (check-1q (format "~a height(~a)" name h) h crt-height e-hs)))

  (define ws 1)
  (define hs 1)

  (check-q* "PS Vita"
            (960 544)
            ((+ 1 1/2 1/4 1/8)
             (+ 1 1/2 1/4 1/8)))
  (check-q* "iPhone 4"
            (960 640)
            ((+ 1 1/2 1/4 1/8)
             (+ 2 1/8 1/16)))
  (check-q* "Normal laptop"
            (1024 640)
            (2
             (+ 2 1/8 1/16)))
  (check-q* "iPhone 5"
            (1136 640)
            ((+ 2 1/8 1/16)
             (+ 2 1/8 1/16)))
  (check-q* "720p"
            (1280 720)
            ((+ 2 1/2)
             (+ 2 1/2)))
  (check-q* "1080p"
            (1920 1080)
            ((+ 3 1/2 1/4)
             (+ 3 1/2 1/4)))
  (check-q* "MacBook Pro Retina, Arch"
            (1440 900)
            ((+ 2 1/2 1/4 1/16)
             (+ 3 1/8))))

(define-shader-source  crt:fragment-source "opengl/crt.fragment.glsl")
(define-shader-source flat:fragment-source "opengl/flat.fragment.glsl")
(define-shader-source  crt:vertex-source   "opengl/crt.vertex.glsl")
(define-shader-source flat:vertex-source   "opengl/flat.vertex.glsl")

(define (make-draw-scaled mode W H)
  (define vertex-source
    (if (eq? mode 'flat)
        flat:vertex-source
        crt:vertex-source))
  (define fragment-source
    (if (eq? mode 'flat)
        flat:fragment-source
        crt:fragment-source))

  (eprintf "You are using OpenGL ~a\n"
           (gl-version))

  (define texture-width W)
  (define texture-height H)

  (define myTexture (u32vector-ref (glGenTextures 1) 0))

  (glBindTexture GL_TEXTURE_2D myTexture)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
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
   (f32vector (* 1. texture-width) (* 1. texture-height)))
  (glUniform2fv
   (glGetUniformLocation shader_program "rubyTextureSize")
   1
   (f32vector (* 1. texture-width) (* 1. texture-height)))

  (glUseProgram 0)

  (define VaoId (u32vector-ref (glGenVertexArrays 1) 0))
  (define VboId (u32vector-ref (glGenBuffers 1) 0))

  (define (draw-scaled actual-screen-width actual-screen-height do-the-drawing)
    ;; Init

    ;; xxx save the old actual-screen-width actual-screen-height and
    ;; only run this if they change
    (define scale
      (* 1.
         (min (quotient* actual-screen-width texture-width)
              (quotient* actual-screen-height texture-height))))

    (define screen-width (* scale texture-width))
    (define screen-height (* scale texture-height))

    (define inset-left (/ (- actual-screen-width screen-width) 2.))
    (define inset-right (+ inset-left screen-width))
    (define inset-bottom (/ (- actual-screen-height screen-height) 2.))
    (define inset-top (+ inset-bottom screen-height))

    (glUseProgram shader_program)
    (glUniform2fv
     (glGetUniformLocation shader_program "rubyOutputSize")
     1
     ;; xxx this might have to be without actual-
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
    (glViewport 0 0 texture-width texture-height)
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

  draw-scaled)

;; xxx
(define (num->pow2 n)
  (inexact->exact
   (ceiling
    (/ (log n)
       (log 2)))))
(module+ test
  (check-equal? (num->pow2 1) 0)
  (check-equal? (num->pow2 2) 1)
  (check-equal? (num->pow2 3) 2)
  (check-equal? (num->pow2 4) 2)
  (check-equal? (num->pow2 5) 3)
  (check-equal? (num->pow2 10) 4))
(define-shader-source VertexShader "opengl/ngl.vertex.glsl")
(define-shader-source FragmentShader "opengl/ngl.fragment.glsl")
(define (make-draw-sprites csd width height)
  (define DrawnMult 6)

  (define SpriteData-count 0)
  (define SpriteData #f)

  (define (install-object! i o)
    (define-syntax-rule (point-install! Horiz Vert j ...)
      (begin
        (set-sprite-data-horiz! o Horiz)
        (set-sprite-data-vert! o Vert)
        (cvector-set! SpriteData (+ (* i 6) j) o)
        ...))
    ;; I once thought I could use a degenerative triangle strip, but
    ;; that adds 2 additional vertices on all but the first and last
    ;; triangles, which would save me exactly 2 vertices total.
    ;;
    ;; xxx GL_ELEMENT_ARRAY_BUFFER?
    (point-install! -1 +1 0)
    (point-install! +1 +1 1 4)
    (point-install! -1 -1 2 3)
    (point-install! +1 -1 5))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))
  (define-syntax-rule (bind-attrib-location Index Name)
    (glBindAttribLocation ProgramId Index (format "in_~a" 'Name)))
  (define-syntax-rule (bind-attrib-location* arg ...)
    (begin (bind-attrib-location . arg) ...))
  ;; XXX It's painful to write this rather than an abstraction.
  (bind-attrib-location*
   [0     dxy]
   [1     mxy]
   [2 theta_a]
   [3 spr_pal]
   [4    lrgb]
   [5 hor_ver])

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
     (λ (left o)
       (add1 left))
     0
     t))

  (define (2D-defaults)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))
  
  (define (show-bytes name w h bs)
    (printf "Loading bytes for ~a (~ax~a):\n" name w h)
    (for ([y (in-range h)])
      (for ([x (in-range w)])
        (define (pixel-ref bs w h bx by i)
          (bytes-ref bs (fx+ (fx* (fx* 4 w) by) (fx+ (fx* 4 bx) i))))
        (printf "(~a,~a,~a,~a)"
                (pixel-ref bs w h x y 0)
                (pixel-ref bs w h x y 1)
                (pixel-ref bs w h x y 2)
                (pixel-ref bs w h x y 3)))
      (printf "\n")))

  (define (load-texture/bytes name internal-type w h external-type bs)    
    (define Id (u32vector-ref (glGenTextures 1) 0))
    (glBindTexture GL_TEXTURE_2D Id)
    (2D-defaults)
    (glTexImage2D GL_TEXTURE_2D
                  0 internal-type
                  w h 0
                  GL_RGBA external-type
                  bs)
    Id)
  ;; xxx premultiply alpha?
  (define (argb->rgba bs)
    (define nbs (make-bytes (bytes-length bs)))
    (for ([i (in-range (fxquotient (bytes-length bs) 4))])
      (define offset (fx* 4 i))
      (bytes-set! nbs (fx+ 0 offset) (bytes-ref bs (fx+ 1 offset)))
      (bytes-set! nbs (fx+ 1 offset) (bytes-ref bs (fx+ 2 offset)))
      (bytes-set! nbs (fx+ 2 offset) (bytes-ref bs (fx+ 3 offset)))
      (bytes-set! nbs (fx+ 3 offset) (bytes-ref bs (fx+ 0 offset))))
    nbs)
  (define (load-texture/argb-bytes name w h argb-bs)
    (define rgba-bs (argb->rgba argb-bs))
    (load-texture/bytes name GL_RGBA8 w h
                        GL_UNSIGNED_BYTE
                        rgba-bs))

  (define SpriteAtlasId
    (load-texture/argb-bytes 'SpriteAtlas
                             (compiled-sprite-db-atlas-size csd)
                             (compiled-sprite-db-atlas-size csd)
                             (compiled-sprite-db-atlas-bs csd)))
  (define PaletteAtlasId
    (load-texture/argb-bytes 'PaletteAtlas
                             PALETTE-DEPTH
                             (compiled-sprite-db-pal-size csd)
                             (compiled-sprite-db-pal-bs csd)))
  (define SpriteIndexId
    (let ()
      (define mapping (compiled-sprite-db-idx->w*h*tx*ty csd))
      (define sprite-index-count (vector-length mapping))
      (define effective-sprite-index-count
        (expt 2 (num->pow2 sprite-index-count)))
      (define idx-bs (make-bytes (* 4 4 sprite-index-count)))
      (define (write-int! w i j)
        (real->floating-point-bytes
         (->fl w) 4 (system-big-endian?)
         idx-bs (+ (* 4 4 i) (* 4 j))))
      (for ([i (in-naturals)]
            [w*h*tx*ty (in-vector mapping)])
        (match-define (vector w h tx ty) w*h*tx*ty)        
        (write-int!  w i 0)
        (write-int!  h i 1)
        (write-int! tx i 2)
        (write-int! ty i 3))
      (load-texture/bytes 'SpriteIndex
                          GL_RGBA32F 1 effective-sprite-index-count
                          GL_FLOAT idx-bs)))

  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)

  (glUseProgram ProgramId)
  (glUniform1i  (glGetUniformLocation ProgramId "SpriteAtlasTex")  0)
  (glUniform1i  (glGetUniformLocation ProgramId "PaletteAtlasTex") 1)
  (glUniform1i  (glGetUniformLocation ProgramId "SpriteIndexTex")  2)
  (glUniform1ui (glGetUniformLocation ProgramId "ViewportWidth")   width)
  (glUniform1ui (glGetUniformLocation ProgramId "ViewportHeight")  height)
  (glUseProgram 0)

  ;; Create VBOs
  (define VaoId (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (define (glVertexAttribIPointer* index size type normalized stride pointer)
    (glVertexAttribIPointer index size type stride pointer))

  (define-syntax-rule
    (define-vertex-attrib-array
      Index Id HowMany Type Floats-before Shorts-before Bytes-before)
    (begin
      (define int? (not (= GL_FLOAT Type)))
      (define byte-offset
        (+ (* 4 Floats-before)
           (* 2 Shorts-before)
           (* 1 Bytes-before)))
      (printf "~a at offset ~v\n" 'Id byte-offset)
      ((if int? glVertexAttribIPointer* glVertexAttribPointer)
       Index HowMany Type
       #f
       (ctype-sizeof _sprite-data)
       byte-offset)
      (glEnableVertexAttribArray Index)))

  (define VboId (u32vector-ref (glGenBuffers 1) 0))

  (glBindBuffer GL_ARRAY_BUFFER VboId)

  (define-syntax-rule
    (define-vertex-attrib-array* arg ...)
    (begin (define-vertex-attrib-array . arg) ...))

  ;; XXX It's painful to write this rather than an abstraction.
  (define-vertex-attrib-array*
    [0     dxy 2 GL_FLOAT          0 0 0]
    [1     mxy 2 GL_FLOAT          2 0 0]
    [2 theta_a 2 GL_FLOAT          4 0 0]
    [3 spr_pal 2 GL_UNSIGNED_SHORT 6 0 0]
    [4    lrgb 4 GL_UNSIGNED_BYTE  6 2 0]
    [5 hor_ver 2 GL_SHORT          6 2 4])

  (glBindBuffer GL_ARRAY_BUFFER 0)

  (glBindVertexArray 0)

  ;; xxx need to deal with layers
  (define (draw layer-config sprite-tree)
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

    (define early-count (count-objects sprite-tree))
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
    (install-objects! sprite-tree)
    (define this-count early-count)
    (glUnmapBuffer GL_ARRAY_BUFFER)
    (glBindBuffer GL_ARRAY_BUFFER 0)

    (glUseProgram ProgramId)

    (glEnable GL_DEPTH_TEST)
    (glClearColor 0.0 0.0 0.0 1.0)

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

(define gui-mode 'gl-core)
(define (stage-draw/dc csd W H)
  (define draw-scaled (make-draw-scaled 'flat W H))
  (define draw-sprites (make-draw-sprites csd W H))
  (λ (layer-config sprite-tree)
    (draw-gl/dc
     (λ (w h)
       (draw-scaled
        w h
        (λ ()
          (draw-sprites layer-config sprite-tree)))))))

(provide
 (ctc:contract-out
  [gui-mode symbol?]
  [stage-draw/dc (stage-backend/c draw/dc/c)]))
