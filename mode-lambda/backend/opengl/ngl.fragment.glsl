#version 330

uniform sampler2D SpriteAtlasTex; 
uniform sampler2D PaletteAtlasTex;

flat in float mid_layer;
flat in float mid_pal_idx;
flat in vec4 mid_color;
in vec2 mid_txy;

out vec4 out_Color;

float adjust_txy (float v) {
  return floor(v + 0.5);
}
 
void main(void) {
  ivec2 txy = ivec2(adjust_txy(mid_txy.x), adjust_txy(mid_txy.y));
  vec4 SpriteColor = texelFetch(SpriteAtlasTex, txy, 0);

  float PaletteOffset = SpriteColor.g * 255;
  ivec2 PalCoord_uv = ivec2( PaletteOffset, mid_pal_idx );
  vec4 PaletteColor = texelFetch(PaletteAtlasTex, PalCoord_uv, 0 );

  SpriteColor = vec4(1.0,0.0,0.0,1.0);
  // xxx is it not getting set or is the screen full of one object?
  // xxx i should implement glReadPixels, dump them, and look at the output and see what is drawn by putting the info i need in it.
  if ( mid_pal_idx > 20.0 ) {
    PaletteColor = vec4(0.0,1.0,0.0,1.0);
  } else {
    PaletteColor = vec4(0.0,0.0,1.0,1.0);
  }
  
  out_Color = (mid_pal_idx == 0.0 ? SpriteColor : PaletteColor);
  
  out_Color.rgb = out_Color.rgb + mid_color.rgb;
  out_Color.a = out_Color.a * mid_color.a;

  // xxx fill the appropriate layer's color
  if ( mid_layer > 1024.0 ) {
    discard;
  }

  if ( out_Color.a == 0.0 ) {
    discard;
  }
}
