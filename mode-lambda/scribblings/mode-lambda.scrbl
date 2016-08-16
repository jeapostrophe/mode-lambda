#lang scribble/manual
@require[(only-in mode-lambda/core
                  LAYERS PALETTE-DEPTH)
         @for-label[racket/base
                    racket/contract/base]]

@title{mode-lambda: the best 2D graphics of the 90s, today!}
@author{Jay McCarthy}

The @racketmodname[mode-lambda] package provides a set of libraries
for creating high-performance 2D graphics. Try running
@exec{racket -l mode-lambda/examples/one} to see what it can
do.

@local-table-of-contents[]

@section{mode-lambda drawing model}

@define[ML @racketmodname[mode-lambda]]

@ML renders rectangular bitmaps (called sprites) in a pixel-perfect
manner on a rendering surface. The size of the rendering surface is
fixed ahead-of-time when the backend is initialized. When drawn, the
rendering surface is then scaled to fit the display environment's
drawing surface. @ML follows an ahead-of-time, staged rendering regime
whereby all color data must be "compiled", using
@racket[compile-sprite-db], before any drawing can occur.

@ML draws sprites on to one of @racket[#,LAYERS] layers. The larger
layers are on-top, with transparent pixels seeping through. @ML makes
no guarantees about the layering and transparency behavior of sprites
within a single layer. Each layer may have an independent center
point, width, scaling coefficients, rotation, wrapping behavior, and
Mode-7 coefficients.

Varying the center point (@tt{cx} & @tt{cy}) over time creates a
scrolling effect given stationary sprites on the layer. When the
scrolling effect is combined with a larger or smaller frame (@tt{hw} &
@tt{hh}) that the rendering surface, this produces a clipping
effect. The scaling coefficients (@tt{mx} & @tt{my}) allow the entire
layer to be zoomed in and out. (A scaling change over time is used in
many SNES RPGs to initiate combat.) The rotation
coefficient (@tt{theta}) rotates the entire layer when it is
drawn. The wrapping behavior (@tt{wrap-x?} & @tt{wrap-y?}) allows
sprites that overlap with the vertical (or horizontal) edge of the
screen to be partly drawn on the opposite side. Any of these changes
could be implemented in software by the @ML client, but the layer
configuration provides a more efficient implementation.

Finally, the Mode-7 coefficients create a perspective effect whereby
the horizon is defined to be a particular Y value (@tt{horiz}) and
then sprites are gradually scaled towards the viewer who has a
particular field of view (@tt{fov}). Specifically: @itemize[

@item{When @tt{mode7-coeff} is @racket[0], then there is no effect.}

@item{When @tt{mode7-coeff} is @racket[1], then the horizon defines
a "ceiling".}

@item{When @tt{mode7-coeff} is @racket[2], then the horizon defines
a "floor". This is the most common usage of Mode-7 and is used in
Mario Kart for the track and many RPGs for the world map.}

@item{When @tt{mode7-coeff} is @racket[3], then the horizon defines
the middle of "cylinder".}

]

It is easiest to understand this effect by looking at some screenshots
of SNES games. (Although, @ML does not support as powerful of a Mode-7
effect, because the parameters cannot be changed per Y line, but all
common uses are supported.)

When @ML draws a sprite, that sprite instance is associated with a
palette of @racket[#,PALETTE-DEPTH] colors. If the palette is
@racket[0], then the sprite bitmap color data is directly consulted
for the color to render. Otherwise, only the green value of the
sprite's color data is used and it is used after scaling it down by
@racket[14] as an offset into the given palette.

Each sprite instance has many drawing paramters. The most important is
its center point (@tt{cx} & @tt{cy}) and which sprite is to be
drawn (@tt{spr-idx}). Next, the layer (@tt{layer}) and
palette (@tt{pal-idx}) can be specified as mentioned previously. The
color of the pixels can be further influenced by a color tint. The
given alpha value (@tt{a}) is multiplied with the sprite/palette
color, while the diffuse colors (@tt{r}, @tt{g}, and @tt{b}) are
added. Finally, each sprite may be independently scaled in the
X (@tt{mx}) or Y (@tt{my}) direction and may be independently
rotated (@tt{theta}).

Each rendering pass of @ML receives two trees of sprite instances. The
first is called the "static" sprites and the second are the "dynamic"
sprites. The static sprites are always drawn first on a given
layer. Aside from this, they are treated equally, but @ML will
guarantee that if a tree is @racket[eq?] to the tree from the last
frame, the GPU's data will not be refreshed. This guarantees that
static level geometry is uploaded once, if it never changes, so @ML
programs should factor their graphics into static and dynamic pieces
for performance.

@ML is extremely memory efficient. Each sprite instance consumes 32
bytes on the CPU and the GPU. If a GPU supported one gigabyte per
second memory transfer, then about 550,000 sprites would be supported
at 60 FPS. Given that most GPUs support many gigabytes per second
memory transfer rates, this means that performance is almost always
dominated by the @ML client's preparation of the sprite trees and @ML
rendering can be treated as free.

@section{mode-lambda static: sprite libraries}
@defmodule[mode-lambda/static]
@require[@for-label[mode-lambda/static
                    file/convertible
                    racket/class
                    racket/gui/base]]

Use this module to construct a database of sprites that your program
will display. This can be run offline and the result saved
persistently with @racket[save-csd!].

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

@defproc[(add-sprite!/bm [db sprite-db?]
                         [n symbol?]
                         [load-bm
                          (-> (or/c path-string? input-port?
                                    (is-a?/c bitmap%)))]
                         [#:palette pal (or/c #f symbol?) #f])
         void?]{

Adds the sprite returned by @racket[load-bm] to @racket[db] using the
name @racket[n]. If @racket[pal] is not false, then the palette named
is consulted to interpret the bitmap pixel values into palette
offsets.}

@defproc[(add-sprite!/file [db sprite-db?]
                           [n symbol?]
                           [file path-string?]
                           [#:palette pal (or/c #f symbol?) #f])
         void?]{

Adds the sprite inside @racket[file] to @racket[db] using the name
@racket[n]. If @racket[pal] is not false, then the palette named is
consulted to interpret the bitmap pixel values into palette offsets.}

@defproc[(add-sprite!/value [db sprite-db?]
                            [n symbol?]
                            [val convertible?]
                            [#:palette pal (or/c #f symbol?) #f])
         void?]{

Adds the sprite @racket[val] to @racket[db] using the name
@racket[n]. If @racket[pal] is not false, then the palette named is
consulted to interpret the bitmap pixel values into palette offsets.}

@defproc[(add-palette! [db sprite-db?]
                       [n symbol?]
                       [cs (listof color?)])
         void?]{

Adds the palette @racket[cs] to @racket[db] with the name @racket[n].}

@defproc[(add-palette!/file [db sprite-db?]
                            [n symbol?]
                            [file path-string?])
         void?]{

Adds the palette inside @racket[file], which is expected to be a
1x@racket[PALETTE-DEPTH] bitmap to @racket[db] with the name
@racket[n].}

@defproc[(save-csd! [cdb compiled-sprite-db?]
                    [path path-string?]
                    [#:debug? debug? boolean? #f])
         void?]{

Saves the compiled database @racket[db] into the directory
@racket[path] with the name @filepath{csd.rktd.gz}. If @racket[debug?]
is @racket[#t], then the sprite atlas and palette is saved in the
directory as PNG files.}

@defproc[(compile-sprite-db [db sprite-db?])
         compiled-sprite-db?]{

Compiles the database @racket[db].}

@section{mode-lambda edition: amazing graphics}
@defmodule[mode-lambda]
@require[@for-label[mode-lambda]]

Use this module to construct scenes for one of the backends. Connect
it to @racketmodname[mode-lambda/static] with @racket[load-csd].

@defthing[LAYERS exact-nonnegative-integer?]{

The number of layers supported by
@racketmodname[mode-lambda]. Currently @racket[#,LAYERS].}

@defthing[PALETTE-DEPTH exact-nonnegative-integer?]{

The number of colors in one palette supported by
    @racketmodname[mode-lambda]. Currently @racket[#,PALETTE-DEPTH].}

@defthing[default-layer-config layer-vector/c]{

The default configuration of layers. You almost certainly don't want
to use it.}

@defproc[(compiled-sprite-db? [x any/c])
         boolean?]{

Identifies values returned by @racket[compile-sprite-db].}

@defproc[(load-csd/bs [bs bytes?])
         compiled-sprite-db?]{

Loads the compiled database from the bytes @racket[bs].}

@defproc[(load-csd [path path-string?])
         compiled-sprite-db?]{

Loads the compiled database from the directory @racket[path].}

@defproc[(sprite-idx [cdb compiled-sprite-db?]
                     [n symbol?])
         (or/c #f exact-nonnegative-integer?)]{

Looks up the index for the sprite @racket[n] in @racket[cdb].}

@defproc[(palette-idx [cdb compiled-sprite-db?]
                      [n symbol?])
         (or/c #f exact-nonnegative-integer?)]{

Looks up the index for the palette @racket[n] in @racket[cdb].}

@defproc[(sprite-width [cdb compiled-sprite-db?]
                       [spr-idx exact-nonnegative-integer?])
         exact-nonnegative-integer?]{
                                     
Returns the width of @racket[spr-idx] in @racket[cdb].}

@defproc[(sprite-height [cdb compiled-sprite-db?]
                        [spr-idx exact-nonnegative-integer?])
         exact-nonnegative-integer?]{
                                     
Returns the height of @racket[spr-idx] in @racket[cdb].}

@defproc[(sprite [cx flonum?]
                 [cy flonum?]
                 [spr-idx exact-nonnegative-integer?]
                 [#:layer layer byte? 0]
                 [#:r r byte? 0]
                 [#:g g byte? 0]
                 [#:b b byte? 0]
                 [#:a a flonum? 1.0]
                 [#:pal-idx pal-idx exact-nonnegative-integer? 0]
                 [#:m m flonum? 1.0]
                 [#:mx mx flonum? m]
                 [#:my my flonum? m]
                 [#:theta theta flonum? 0.0])
         sprite-data?]{

Returns a sprite instance.}

@defproc[(sprite-data? [x any/c]) boolean?]{Identifies sprite data.}
@defproc[(sprite-data-dx [s sprite-data?]) flonum?]{Returns X offset.}
@defproc[(sprite-data-dy [s sprite-data?]) flonum?]{Returns Y offset.}
@defproc[(sprite-data-mx [s sprite-data?]) flonum?]{Returns X scale.}
@defproc[(sprite-data-my [s sprite-data?]) flonum?]{Returns Y scale.}
@defproc[(sprite-data-spr [s sprite-data?]) exact-nonnegative-integer?]{Returns sprite index.}

@defproc[(layer [cx flonum?]
                [cy flonum?]
                [#:hw hw flonum? +inf.0]
                [#:hh hh flonum? +inf.0]
                [#:wrap-x? wrap-x? boolean? #f]
                [#:wrap-y? wrap-y? boolean? #f]
                [#:mx mx flonum? 1.0]
                [#:my my flonum? 1.0]
                [#:theta theta flonum? 0.0]
                [#:mode7 mode7-coeff flonum? 0.0]
                [#:horizon horiz flonum? 0.0]
                [#:fov fov flonum? 1.0])
         layer-data?]{

Returns a layer configuration.}

@section{mode-lambda color: basic color theory}
@require[(for-label mode-lambda/color)]
@defmodule[mode-lambda/color]

This module defines helpers for creating color palettes for
@racketmodname[mode-lambda].

@defthing[GRAY color?]{The color gray.}
@defthing[TRANSPARENT color?]{The transparent color.}
@defthing[BLACK color?]{The color black.}
@defthing[WHITE color?]{The color white.}

@defproc[(argb [a byte?] [r byte?] [g byte?] [b byte?])
         color?]{Constructs a color object.}

@defproc[(color? [x any/c]) boolean?]{Identifies colors.}

@defproc[(alpha [c color?]) byte?]{Extracts the alpha value of @racket[c].}
@defproc[(red [c color?]) byte?]{Extracts the red value of @racket[c].}
@defproc[(green [c color?]) byte?]{Extracts the green value of @racket[c].}
@defproc[(blue [c color?]) byte?]{Extracts the blue value of @racket[c].}

@defproc[(color->palette [base color?]) (listof color?)]{Returns a
list of colors where the first color is @racket[TRANSPARENT], the next
is @racket[BLACK], followed by 7 shades of @racket[base],
@racket[base], 7 tints of @racket[base], and then @racket[WHITE].}

@defproc[(color->tint [base color?] [how-many exact-nonnegative-integer?])
         (listof color?)]{Returns @racket[how-many] tints of
@racket[base]. Tints are brighter than their base color.}

@defproc[(color->shades [base color?] [how-many exact-nonnegative-integer?])
         (listof color?)]{Returns @racket[how-many] shades of
@racket[base]. Shades are darker than their base color.}

@defproc[(color-wheel [how-many exact-nonnegative-integer?]
                      [#:s s (real-in 0.0 1.0) 1.0]
                      [#:b b (real-in 0.0 1.0) 1.0])
         (listof color?)]{

Takes @racket[how-many] evenly spaced samples of the color wheel with
      saturation @racket[s] and brightness @racket[b].}

@defproc[(complement-idxs [how-many exact-nonnegative-integer?])
         (listof vector?)]{Returns a list of 2-vectors that contain
indexes into a color wheel of size @racket[how-many] where each pair
are complementary colors.}

@defproc[(analogous-idxs [how-many exact-nonnegative-integer?])
         (listof vector?)]{Returns a list of 3-vectors that contain
indexes into a color wheel of size @racket[how-many] where each triple
are analogous colors.}

@defproc[(triadic-idxs [how-many exact-nonnegative-integer?])
         (listof vector?)]{Returns a list of 3-vectors that contain
indexes into a color wheel of size @racket[how-many] where each triple
are triadic colors.}

@defproc[(split-complementary-idxs [how-many exact-nonnegative-integer?])
         (listof vector?)]{Returns a list of 3-vectors that contain
indexes into a color wheel of size @racket[how-many] where each triple
are split complementary colors.}

@defproc[(tetradic-idxs [how-many exact-nonnegative-integer?])
         (listof vector?)]{Returns a list of 4-vectors that contain
indexes into a color wheel of size @racket[how-many] where each quadruple
are tetradic colors.}

@defproc[(square-idxs [how-many exact-nonnegative-integer?])
         (listof vector?)]{Returns a list of 4-vectors that contain
indexes into a color wheel of size @racket[how-many] where each quadruple
are square colors.}

@defproc[(polygon-idxs [n exact-nonnegative-integer?]
                       [how-many exact-nonnegative-integer?])
         (listof vector?)]{Returns a list of @racket[n]-vectors that
contain indexes into a color wheel of size @racket[how-many] where
each vector of colors are on the vertix of a regular
@racket[n]-gon. (This is a generalization of @racket[triadic-idxs] and
@racket[square-idxs].)}


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

@section{mode-lambda shot: screenshot helpers}
@defmodule[mode-lambda/shot]
@require[@for-label[mode-lambda/shot
                    mode-lambda/backend/gl]]

@defproc[(screenshot-in-dir! [p path-string?])
         (-> exact-nonnegative-integer?
             exact-nonnegative-integer?
             exact-nonnegative-integer?
             bytes?
             void?)]{

Saves the screenshots that @racket[gl-screenshot!] generates in @racket[p].}
