#lang scribble/manual
@require[@for-label[racket/base
                    lux/chaos/gui
                    mode-lambda
                    racket/contract/base
                    mode-lambda/shot
                    mode-lambda/backend/gl]]

@title{mode-lambda gl: premier backend}
@defmodule[mode-lambda/backend/gl]

This is the production backend for @racketmodname[mode-lambda]. It is
pretty fast, but kind of complicated.

@defthing[stage-draw/dc
          (->i ([cdb compiled-sprite-db?]
                [render-width exact-nonnegative-integer?]
                [render-height exact-nonnegative-integer?])
               (->i ([layer-config layer-vector/c]
                     [static-st any/c]
                     [dynamic-st any/c])
                    (->i ([draw-width exact-nonnegative-integer?]
                          [draw-height exact-nonnegative-integer?]
                          [dc any/c])
                         any)))]{

Prepares a function that accepts rendering states and returns a
function that draws that rendering state.}

@defthing[gui-mode symbol?]{

A symbol to be sent to @racket[make-gui] of @racketmodname[lux/chaos/gui].}

@defthing[gl-backend-version (parameter/c (one-of '3.3 'es3.2))]{

Sets the desired version of OpenGL to use.}

@defthing[gl-filter-mode (parameter/c symbol?)]{

This parameter controls the screen filter used to magnify the render
surface on to the drawing surface. The default mode @racket['std],
uses sharp pixel-duplicating filtering. The @racket['crt] mode
simulates a CRT.}

@defthing[gl-screenshot! (parameter/c (-> exact-nonnegative-integer?
                                          exact-nonnegative-integer?
                                          exact-nonnegative-integer?
                                          bytes?
                                          void?))]{

If this value is not @racket[#f], then it is called once for each
layer with (a) the layer identifier, (b) the width of the layer, (c)
the height of the layer, and (d) the raw pixel values in ARGB
order. Enabling this drastically lowers rendering performance because
it forces a pixel buffer read and synchronization.

@racket[(screenshot-in-dir! _dir)] from
@racketmodname[mode-lambda/shot] is an ideal value for this
parameter.}
