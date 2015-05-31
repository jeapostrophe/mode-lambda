#lang scribble/manual
@require[@for-label[racket/base
                    lux/chaos/gui
                    racket/contract/base
                    mode-lambda
                    (except-in mode-lambda/text
                               load-font!)
                    mode-tau]]

@title{mode-tau: the text terminal of the 1970s}
@defmodule[mode-tau]

@racketmodname[mode-tau] is a variant of @racketmodname[mode-lambda]
that is just for laying out grayscale bitmaps in an axis-aligned
fashion like a traditional text terminal. It is optimized for that
purpose, but uses similar concepts of preparing all the glyphs that
will be displayed, compiling the glyph database, and then laying it
out. The only customization that exists at drawing time is what two
colors define the "light" and "dark" side of a single glyph's pixel
data.

@defthing[stage-draw/dc
          (->i ([cdb compiled-sprite-db?])
               (->i ([bgr byte?]
                     [bgg byte?]
                     [bgb byte?]
                     [glyphs any/c])
                    (->i ([draw-width exact-nonnegative-integer?]
                          [draw-height exact-nonnegative-integer?]
                          [dc any/c])
                         any)))]{

Prepares a function that accepts rendering states (background red
color, background green color, background blue color, and tree of
glyphs) and returns a function that draws that rendering state.}

@defproc[(glyph [dx flonum?] [dy flonum?]
                [glyph-idx exact-nonnegative-integer?]
                [#:fgr fgr byte? 0]
                [#:fgg fgg byte? 0]
                [#:fgb fgb byte? 0]
                [#:bgr bgr byte? 255]
                [#:bgg bgg byte? 255]
                [#:bgb bgb byte? 255])
         any/c]{

Constructs a glyph object for drawing. The glyph drawn is the one
identified by @racket[glyph-idx] and its center is
@racket[dx]x@racket[dy]. The glyph's white pixels are drawn using the
background color (@racket[bgr] for red, @racket[bgg] for green,
@racket[bgb] for blue) and the glyph's black pixels are drawn using
the foreground color (@racket[fgr] for red, @racket[fgg] for green,
@racket[fgb] for blue.)}

@defproc[(make-glyph-db) sprite-db?]{

Constructs a glyph database for storing fonts.}

@defproc[(load-font! [db sprite-db?]
                     [#:size size (real-in 0.0 1024.0) 12]
                     [#:face face (or/c string? #f) #f]
                     [#:family family 	
                      (or/c 'default 'decorative 'roman 'script
                            'swiss 'modern 'symbol 'system)
                      'default]
                     [#:style style (or/c 'normal 'italic 'slant) 'normal]
                     [#:weight weight (or/c 'normal 'bold 'light) 'normal]
                     [#:underlined? underlined? any/c #f]
                     [#:smoothing smoothing 	
                      (or/c 'default 'partly-smoothed
                            'smoothed 'unsmoothed)
                      'default]
                     [#:size-in-pixels? size-in-pixels any/c #f]
                     [#:hinting hinting (or/c 'aligned 'unaligned) 'aligned]
                     [#:alphabet alphabet (listof char?) *ALL-ASCII*]) font?]{

Uses most of its arguments to call @racket[make-font] from
@racketmodname[racket/draw] to construct a font object, which it uses
to add all of the characters in @racket[alphabet] to @racket[db] and
returns a font object for use with @racket[font-glyph-idx].}

@defproc[(compile-glyph-db [db sprite-db?]) compiled-sprite-db?]{

Compiles a glyph database into an efficient format for rendering.}

@defproc[(font-glyph-idx [the-font font?]
                         [cdb compiled-sprite-db?]
                         [char char?])
         exact-nonnegative-integer?]{

Looks up the character identifier of the character @racket[char] of
the font @racket[the-font] in the databvase @racket[cdb].}

@defproc[(glyph-width [cdb compiled-sprite-db?]
                      [ci exact-nonnegative-integer?])
         flonum?]{

Returns the width of the character identified by @racket[ci].}

@defproc[(glyph-height [cdb compiled-sprite-db?]
                      [ci exact-nonnegative-integer?])
         flonum?]{

Returns the height of the character identified by @racket[ci].}

@defthing[gui-mode symbol?]{

A symbol to be sent to @racket[make-gui] of @racketmodname[lux/chaos/gui].}
