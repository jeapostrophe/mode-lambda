#version 330

uniform sampler2D SpriteIndexTex;

in vec2 in_dxy;
in vec2 in_mxy;
in vec2 in_theta_a;
in uvec2 in_spr_pal;
in uvec4 in_lrgb;
in ivec2 in_hor_ver;

uniform uint ViewportWidth;
uniform uint ViewportHeight;

flat out float mid_layer;
flat out float mid_pal_idx;
flat out vec4 mid_color;
out vec2 mid_txy;

mat4 glRotate( float angle, float x, float y, float z ) {
  float c = cos(angle);
  float s = sin(angle);
  return mat4( x*x*(1-c) + c, x*y*(1-c) - z*s, x*z*(1-c) + y*s, 0.0,
               y*x*(1-c) + z*s, y*y*(1-c) + c, y*z*(1-c) - x*s, 0.0,
               x*z*(1-c) - y*s, y*z*(1-c) + x*s, z*z*(1-c)+c, 0.0,
               0.0, 0.0, 0.0, 1.0);
}

mat4 glOrtho( float left, float right, float bottom, float top, float nearVal, float farVal ) {
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

void main(void) {
  float in_dx = in_dxy.x;
  float in_dy = in_dxy.y;
  float in_mx = in_mxy.x;
  float in_my = in_mxy.y;
  float in_theta = in_theta_a[0];
  float in_a = in_theta_a[1];
  uint in_spr_idx = in_spr_pal[0];
  uint in_pal_idx = in_spr_pal[1];
  uint in_layer = in_lrgb[0];
  uint in_r = in_lrgb[1];
  uint in_g = in_lrgb[2];
  uint in_b = in_lrgb[3];
  int in_horiz = in_hor_ver[0];
  int in_vert = in_hor_ver[1];

  vec4 in_TexCoord = texelFetch(SpriteIndexTex, ivec2(0, in_spr_idx), 0);
  float in_sw = in_TexCoord.x;
  float in_sh = in_TexCoord.y;
  float in_stx = in_TexCoord.z;
  float in_sty = in_TexCoord.w;

  gl_Position =
    vec4(in_horiz * in_sw * 0.5 * in_mx,
         in_vert * in_sh * 0.5 * in_my,
         0.0, 1.0)
    * glRotate(in_theta, 0.0, 0.0, 1.0)
    * glTranslate(in_dx, in_dy, 0.0)
    * glOrtho(0.0, ViewportWidth,
              0.0, ViewportHeight,
              1.0, -1.0);

  mid_layer = in_layer;
  mid_pal_idx = in_pal_idx;
  mid_color = vec4(in_r / 255.0, in_g / 255.0, in_b / 255.0, in_a);
  mid_txy =
    vec2(in_sw + ((in_horiz + 1.0)/2.0) * in_stx,
         in_sh + ((in_vert - 1.0)/-2.0) * in_sty);
}
