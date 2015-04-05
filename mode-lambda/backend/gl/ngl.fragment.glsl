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

out vec4 out_Color[@LAYERS];

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

  // Colors are not pre-multiplied (I don't do this in the software
  // one and I don't know why OpenGL seems to require it)
  PixelColor.rgb = PixelColor.a * PixelColor.rgb;

  vec4 fin_Color;
  
  fin_Color.a = PixelColor.a * Color.a;
  fin_Color.rgb = PixelColor.rgb + Color.rgb;
  
  if ( fin_Color.a == 0.0 ) {
    discard;
  }

  int iLayer = int(floor(Layer));
  // xxx remove this hack
  iLayer = 0;
  out_Color[iLayer] = fin_Color;
}
