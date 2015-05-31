#lang scribble/manual
@require[@for-label[racket/base]]

@title{mode-lambda: the best 2D graphics of the 90s, today!}
@author{Jay McCarthy}

The @tt{mode-lambda} package provides a set of libraries for creating
high-performance 2D graphics.

@local-table-of-contents[]

@section{mode-lambda color: basic color theory}
@defmodule[mode-lambda/color]

XXX

@section{mode-lambda edition: amazing graphics}
@defmodule[mode-lambda]
@require[(only-in mode-lambda/core
                  LAYERS PALETTE-DEPTH)
         @for-label[mode-lambda]]

@defthing[LAYERS exact-nonnegative-integer?]{

The number of layers supported by
@racketmodname[mode-lambda]. Currently @racket[#,LAYERS].}

@defthing[PALETTE-DEPTH exact-nonnegative-integer?]{

The number of colors in one palette supported by
    @racketmodname[mode-lambda]. Currently @racket[#,PALETTE-DEPTH].}

@defthing[default-layer-config layer-vector/c]{

The default configuration of layers.}

@defproc[(make-sprite-db) sprite-db?]{

Returns a sprite database.}

@defproc[(sprite-db? [x any/c]) boolean?]{

Identifies values returned by @racket[make-sprite-db].}

@defproc[(sprite-attributes? [x any/c]) boolean?]{

Identifies a vector with 5 elements. The first (@tt{n}) is a
@racket[symbol?], the second (@tt{pal}) is either @racket[#f] or a
@racket[symbol?], the third (@tt{w}) and fouth (@tt{h}) are 16-bit
@racket[exact-nonnegative-integer?], and the fifth (@tt{bs}) is a
@racket[bytes?] describing the ARGB pixels of the sprite. @tt{bs} must
be @racket[(* 4 w h)] long. If @tt{pal} is not false, then the palette
@tt{pal} is used to interpret the pixel values into palette offsets.}

@defproc[(add-sprite! [db sprite-db?]
                      [load-spr (-> sprite-attributes?)])
         void?]{
                
Adds the sprite returned by @racket[load-spr] to @racket[db].}

XXX

@section{mode-lambda text: text layout}
@defmodule[mode-lambda/text]
@require[@for-label[mode-lambda/text]]

This module captures some convenience functions for adding font glyphs
to the sprite database and laying out horizontal text.

@defthing[*ALL-ASCII* (listof char?)]{
                                      
A list of all graphical ASCII characters.}

@defproc[(font? [x any/c]) boolean?]{

Identifies objects returned from @racket[load-font!].}

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

@defproc[(font-char-idx [the-font font?]
                        [cdb compiled-sprite-db?]
                        [char char?])
         exact-nonnegative-integer?]{

Looks up the character identifier of the character @racket[char] of
the font @racket[the-font] in the databvase @racket[cdb].}

@defproc[(make-text-renderer [f font?]
                             [cdb compiled-sprite-db?])
         (->i ([text string?]
               [tx real?]
               [ty real?])
              ([#:layer l byte?]
               [#:mx mx real?]
               [#:my my real?]
               [#:r r byte?]
               [#:g g byte?]
               [#:b b byte?]
               [#:a a (real-in 0.0 1.0)])
              any/c)]{

Returns a tree of sprites for the glyphs of the string @racket[text]
such that the center is (@racket[tx], @racket[ty]). The sprite are
created with the sprite parameters given in the optional arguments.}

@include-section["backend-gl.scrbl"]
@include-section["backend-software.scrbl"]

