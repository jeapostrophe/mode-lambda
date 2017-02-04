@glsl-include["lib.glsl"]

uniform sampler2D SpriteAtlasTex; 
uniform sampler2D PaletteAtlasTex;

// None of these are "flat" because they are the same on all vertices
// anyways, except TexCoord which needs to be interpolated across the
// triangle.
in vec4 Color;
in vec2 TexCoord;
in float Palette;
in float Layer;

@in[i (in-range how-many-layers)]{
  layout (location = @i) out vec4 out_Color@i;
}

void main(void)
{
  vec4 SpriteColor;

  SpriteColor =
    // This is what it should be defined as
    //  texture(SpriteAtlasTex,
    //          TexCoord / float(textureSize(SpriteAtlasTex, 0).x))
    // But it doesn't work on some ES devices I have, so we do this instead:
    texelFetch(SpriteAtlasTex,
               ivec2( trunc(TexCoord.x), trunc(TexCoord.y) ),
               0)
  ;

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

  vec4 blank_Color = vec4(0.0,0.0,0.0,0.0000001);
  
  int iLayer = int(floor(Layer));
  @in[i (in-range how-many-layers)]{
    out_Color@i = (iLayer == @i) ? fin_Color : blank_Color;
  }
}
