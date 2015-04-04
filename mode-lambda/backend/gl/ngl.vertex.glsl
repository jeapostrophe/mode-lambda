#version 330

mat4 glRotate( float angle, float x, float y, float z ) {
  float c = cos(angle);
  float s = sin(angle);
  return mat4( x*x*(1-c) + c, x*y*(1-c) - z*s, x*z*(1-c) + y*s, 0.0,
               y*x*(1-c) + z*s, y*y*(1-c) + c, y*z*(1-c) - x*s, 0.0,
               x*z*(1-c) - y*s, y*z*(1-c) + x*s, z*z*(1-c)+c, 0.0,
               0.0, 0.0, 0.0, 1.0);
}

mat4 glOrtho( float left, float right, float bottom, float top,
              float nearVal, float farVal ) {
  float t_x = - (right + left) / (right - left);
  float t_y = - (top + bottom) / (top - bottom);
  float t_z = - (farVal + nearVal) / (farVal - nearVal);
  return mat4( 2.0 / right - left, 0.0, 0.0, t_x,
               0.0, 2.0 / top - bottom, 0.0, t_y,
               0.0, 0.0, -2 / farVal - nearVal, t_z,
               0.0, 0.0, 0.0, 1.0 );
}

mat4 glTranslate( float x, float y, float z ) {
  return mat4(1.0, 0.0, 0.0, x,
              0.0, 1.0, 0.0, y,
              0.0, 0.0, 1.0, z,
              0.0, 0.0, 0.0, 1.0);
}

uniform sampler2D SpriteIndexTex;

in vec2 in_DX_DY;
in vec4 in_MX_MY_THETA_A;
in uvec2 in_SPR_PAL;
in uvec4 in_LAYER_R_G_B;
in ivec2 in_HORIZ_VERT;

uniform uint ViewportWidth;
uniform uint ViewportHeight;

#define LAYERS 8

uniform LayerConfigBlock
{
  float cx;
  float cy;
  float hw;
  float hh;
  float mx;
  float my;
  float theta;
  float mode7coeff;
  float horizon;
  float fov;
  uint wrapxp;
  uint wrapyp;
} LayerConfig[LAYERS];

out vec4 Color;
out vec2 TexCoord;
out float Palette;

void main(void)
{
  float    dx = in_DX_DY.x;
  float    dy = in_DX_DY.y;
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

  vec4 in_TexCoord =
    texelFetch(SpriteIndexTex, ivec2(0, spr), 0);

  float  w = in_TexCoord.x;
  float  h = in_TexCoord.y;
  float tx = in_TexCoord.z;
  float ty = in_TexCoord.w;

  // xxx use LayerConfig

  Color = vec4(r / 255.0, g / 255.0, b / 255.0, a);
  gl_Position =
      vec4(horiz * w * 0.5 * mx, vert * h * 0.5 * my, 0.0, 1.0)
    * glRotate(theta, 0.0, 0.0, 1.0)
    * glTranslate(dx, dy, 0.0)
    * glOrtho(0.0, ViewportWidth,
              0.0, ViewportHeight,
              1.0, -1.0);
  TexCoord =
    vec2(tx + ((horiz + 1.0)/+2.0) * w,
         ty + (( vert - 1.0)/-2.0) * h);
  Palette = pal;
}
