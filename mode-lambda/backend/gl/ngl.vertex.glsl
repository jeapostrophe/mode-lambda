#version 410
@glsl-include["lib.glsl"]

uniform sampler2D SpriteIndexTex;

in vec2 in_DX_DY;
in vec4 in_MX_MY_THETA_A;
in uvec2 in_SPR_PAL;
in uvec4 in_LAYER_R_G_B;
in ivec2 in_HORIZ_VERT;

uniform uint ViewportWidth;
uniform uint ViewportHeight;

uniform sampler2D LayerConfigTex;

out vec4 Color;
out vec2 TexCoord;
out float Palette;
out float Layer;

void main(void)
{
  float    dx = in_DX_DY.x;
  float    dy = in_DX_DY.y;
  uint  layer = in_LAYER_R_G_B.x;
  uint      r = in_LAYER_R_G_B.y;
  uint      g = in_LAYER_R_G_B.z;
  uint      b = in_LAYER_R_G_B.w;
  float     a = in_MX_MY_THETA_A.w;
  float    mx = in_MX_MY_THETA_A.x;
  float    my = in_MX_MY_THETA_A.y;
  float theta = in_MX_MY_THETA_A.z;
  uint    pal = in_SPR_PAL.y;
  uint    spr = in_SPR_PAL.x;
  int   horiz = in_HORIZ_VERT.x;
  int    vert = in_HORIZ_VERT.y;

  vec4 LC_CX_CY_HW_HH =
    texelFetch(LayerConfigTex, ivec2(0, layer), 0);
  float Lcx = LC_CX_CY_HW_HH.x;
  float Lcy = LC_CX_CY_HW_HH.y;
  float Lhw = LC_CX_CY_HW_HH.z;
  float Lhh = LC_CX_CY_HW_HH.w;
  float Lw = 2 * Lhw;
  float Lh = 2 * Lhh;
  vec4 LC_MX_MY_THETA_MODE7COEEF =
    texelFetch(LayerConfigTex, ivec2(1, layer), 0);
  float Lmx = LC_MX_MY_THETA_MODE7COEEF.x;
  float Lmy = LC_MX_MY_THETA_MODE7COEEF.y;
  float Ltheta = LC_MX_MY_THETA_MODE7COEEF.z;
  vec4 LC_HORIZON_FOV_WRAPXP_WRAPYP =
    texelFetch(LayerConfigTex, ivec2(1, layer), 0);
  float wrapxp = LC_HORIZON_FOV_WRAPXP_WRAPYP.z;
  float wrapyp = LC_HORIZON_FOV_WRAPXP_WRAPYP.w;

  // xxx use wrapxp and wrapyp -- the software renderer uses these to
  // decide whether to mess with the points. i really need to generate
  // 4 times more vertices and add two more parameters that are
  // bools. it's not normally possible to "discard" a vertex, but i
  // could set the alpha to 0 or i could set w to 0, so it is
  // projected out to infinity.

  vec4 in_TexCoord =
    texelFetch(SpriteIndexTex, ivec2(0, spr), 0);

  float  w = in_TexCoord.x;
  float  h = in_TexCoord.y;
  float tx = in_TexCoord.z;
  float ty = in_TexCoord.w;

  Color = vec4(r / 255.0, g / 255.0, b / 255.0, a);
  
  gl_Position =
      vec4(horiz * w * 0.5 * mx * Lmx, vert * h * 0.5 * my * Lmy, 0.0, 1.0)
    * glRotate(theta, 0.0, 0.0, 1.0)
    * glTranslate(dx, dy, 0.0)
    // xxx These might be Lhw and Lhh
    * glTranslate(-1.0 * ViewportWidth / 2.0, -1.0 * ViewportHeight / 2.0, 0.0)
    * glRotate(Ltheta, 0.0, 0.0, 1.0)
    * glTranslate(Lcx, Lcy, 0.0)
    * glOrtho(0.0, ViewportWidth,
              0.0, ViewportHeight,
              1.0, -1.0)
    ;
  TexCoord =
    vec2(tx + ((horiz + 1.0)/+2.0) * w,
         ty + (( vert - 1.0)/-2.0) * h);
  Palette = pal;
  Layer = layer;
}
