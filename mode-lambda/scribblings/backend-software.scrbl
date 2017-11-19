#lang scribble/manual
@require[@for-label[racket/base
                    lux/chaos/gui
                    mode-lambda
                    racket/contract/base
                    mode-lambda/backend/software]]

@title{mode-lambda software: discount backend}
@defmodule[mode-lambda/backend/software]

This is the reference backend for @racketmodname[mode-lambda]. It is
very slow, and a little complicated, but easier to test than
@racketmodname[mode-lambda/backend/gl].

@defthing[stage-draw/dc
          (->i ([cdb compiled-sprite-db?]
                [render-width exact-nonnegative-integer?]
                [render-height exact-nonnegative-integer?]
                [layers byte?])
               (->i ([layer-config (vectorof layer-data?)]
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

@defthing[software-bitmap-path (parameter/c path-string?)]{
                                                           
This parameter controls the path that the post-rendering bitmap is
saved to disk. The default is to not save screenshots.}
