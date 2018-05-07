@glsl-include["lib.glsl"]

uniform float ActiveLayer;
uniform sampler2D SpriteAtlasTex; 
uniform sampler2D PaletteAtlasTex;

// None of these are "flat" because they are the same on all vertices
// anyways, except TexCoord which needs to be interpolated across the
// triangle.
in vec4 Color;
in vec2 TexCoord;
in float Palette;
in float Layer;

out vec4 out_Color;

void main(void)
{
  vec4 SpriteColor;

  vec2 texSize = textureSize(SpriteAtlasTex, 0);

  if (@(if (gl-es?) "false" "true")) {
    SpriteColor =
    // This is what it should be defined as
    texture(SpriteAtlasTex,
            vec2(TexCoord.x / texSize.x,
                 TexCoord.y / texSize.y));
  } else {
    SpriteColor =
    // But it doesn't work on some ES devices I have, so we do this instead:
    // Note: this disables smoothing if active
    texelFetch(SpriteAtlasTex,
               ivec2( trunc(TexCoord.x), trunc(TexCoord.y) ),
               0);
  }

  vec4 PixelColor;
  
  if ( Palette == 0.0 ) {
    PixelColor = SpriteColor;
  } else {
    float PaletteOffset = SpriteColor.g * 255.0 / 14.0;
    ivec2 PalCoord_uv = ivec2( PaletteOffset, Palette );
    PixelColor = texelFetch(PaletteAtlasTex, PalCoord_uv, 0 );
  }

  vec4 fin_Color;

  // Color.rgb is the #:r, #:b, #:g passed to the (sprite) call
  // PixelColor is from the sprite, is premultiplied alpha

  fin_Color.a = PixelColor.a * Color.a;

  // add Color.rgb to sprite color, but only where the sprite is opaque
  fin_Color.rgb = PixelColor.rgb + Color.rgb * PixelColor.a;

  // Adding could have pushed rgb over alpha, cap it to maintain
  // premultiplied alpha
  fin_Color.rgb = min(fin_Color.rgb, PixelColor.a);

  // scale final color by #:a passed to (sprite)
  fin_Color.rgb *= Color.a;

  vec4 blank_Color = vec4(0.0,0.0,0.0,0.0);
  
  int iLayer = int(floor(Layer));
  int iActiveLayer = int(floor(ActiveLayer));
  out_Color = (iLayer == iActiveLayer) ? fin_Color : blank_Color;
}
