#lang scribble/manual
@require[@for-label[racket/base
                    lux/chaos/gui
                    mode-lambda
                    racket/contract/base
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

@defthing[gl-filter-mode (parameter/c symbol?)]{

This parameter controls the screen filter used to magnify the render
surface on to the drawing surface. The default mode @racket['std],
uses sharp pixel-duplicating filtering. The @racket['crt] mode
simulates a CRT.}

@defthing[gl-screenshot-dir (parameter/c path-string?)]{

This parameter controls the directory where screenshots post-rendering
are saved. The default value is to not save screenshots. Enabling
saving drastically lowers rendering performance because it forces a
pixel buffer read and synchronization.}
