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
  if (true) {
    SpriteColor =
    // This is what it should be defined as
    texture(SpriteAtlasTex,
            vec2(TexCoord.x / texSize.x,
                 TexCoord.y / texSize.y));
  } else {
    SpriteColor =
    // But it doesn't work on some ES devices I have, so we do this instead:
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

  fin_Color.a = PixelColor.a * Color.a;
  fin_Color.rgb = PixelColor.rgb + Color.rgb;
  fin_Color.rgb = min(fin_Color.rgb, fin_Color.a);

  vec4 blank_Color = vec4(0.0,0.0,0.0,0.0000001);
  
  int iLayer = int(floor(Layer));
  int iActiveLayer = int(floor(ActiveLayer));
  out_Color = (iLayer == iActiveLayer) ? fin_Color : blank_Color;
}
