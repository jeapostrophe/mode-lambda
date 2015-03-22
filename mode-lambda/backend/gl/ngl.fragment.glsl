#version 330

uniform sampler2D SpriteAtlasTex; 
uniform sampler2D PaletteAtlasTex;

// None of these are "flat" because they are the same on all vertices
// anyways, except TexCoord which needs to be interpolated across the
// triangle.
in vec4 Color;
in vec2 TexCoord;
in float Palette;
out vec4 out_Color;

float clampit ( float v ) {
  return floor(v)+0.5;
}
 
void main(void)
{
  vec4 PixelColor;  
  
  ivec2 TexCoord_uv = ivec2(clampit(TexCoord.x), clampit(TexCoord.y));
  vec4 SpriteColor = texelFetch(SpriteAtlasTex, TexCoord_uv, 0);

  if ( Palette == 0.0 ) {
    PixelColor = SpriteColor;
  } else {
    float PaletteOffset = SpriteColor.g * 255;
    ivec2 PalCoord_uv = ivec2( PaletteOffset, Palette );
    PixelColor = texelFetch(PaletteAtlasTex, PalCoord_uv, 0 );
  }
  
  // Colors are not pre-multiplied
  PixelColor.rgb = PixelColor.a * PixelColor.rgb;
  
  //out_Color.a = PixelColor.a * Color.a;
  //out_Color.r = PixelColor.r + Color.r;
  //out_Color.g = PixelColor.g + Color.g;
  //out_Color.b = PixelColor.b + Color.b;
  out_Color = PixelColor + Color;
  
  if ( out_Color.a == 0.0 ) {
    discard;
  }
}
