#version 330

uniform sampler2D SpriteAtlasTex; 
uniform sampler2D PaletteAtlasTex;

// None of these are "flat" because they are the same on all vertices
// anyways, except TexCoord which needs to be interpolated across the
// triangle.
in vec4 Color;
in vec2 TexCoord;
in float Palette;
in float Layer;

// xxx use macro
layout (location = 0) out vec4 out_Color0;
layout (location = 1) out vec4 out_Color1;
layout (location = 2) out vec4 out_Color2;
layout (location = 3) out vec4 out_Color3;
layout (location = 4) out vec4 out_Color4;
layout (location = 5) out vec4 out_Color5;
layout (location = 6) out vec4 out_Color6;
layout (location = 7) out vec4 out_Color7;

float clampx ( float v ) { return floor(v) + 0.5; }
float clampy ( float v ) { return floor(v) - 0.5; }
 
void main(void)
{
  vec4 PixelColor;
  
  ivec2 TexCoord_uv = ivec2(clampx(TexCoord.x), clampy(TexCoord.y));
  vec4 SpriteColor = texelFetch(SpriteAtlasTex, TexCoord_uv, 0);

  if ( Palette == 0.0 ) {
    PixelColor = SpriteColor;
  } else {
    float PaletteOffset = SpriteColor.g * 255;
    ivec2 PalCoord_uv = ivec2( PaletteOffset, Palette );
    PixelColor = texelFetch(PaletteAtlasTex, PalCoord_uv, 0 );
  }

  vec4 fin_Color;
  
  fin_Color.a = PixelColor.a * Color.a;
  fin_Color.rgb = PixelColor.rgb + Color.rgb;

  vec4 blank_Color = vec4(0.0,0.0,0.0,0.0);
  // xxx use macro
  out_Color0 = blank_Color;
  out_Color1 = blank_Color;
  out_Color2 = blank_Color;
  out_Color3 = blank_Color;
  out_Color4 = blank_Color;
  out_Color5 = blank_Color;
  out_Color6 = blank_Color;
  out_Color7 = blank_Color;
  
  int iLayer = int(floor(Layer));
  // xxx use macro
  if (iLayer == 0) { out_Color0 = fin_Color; }
  if (iLayer == 1) { out_Color1 = fin_Color; }
  if (iLayer == 2) { out_Color2 = fin_Color; }
  if (iLayer == 3) { out_Color3 = fin_Color; }
  if (iLayer == 4) { out_Color4 = fin_Color; }
  if (iLayer == 5) { out_Color5 = fin_Color; }
  if (iLayer == 6) { out_Color6 = fin_Color; }
  if (iLayer == 7) { out_Color7 = fin_Color; }
}
