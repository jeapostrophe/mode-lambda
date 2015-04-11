mat4 glScale( float mx, float my, float mz ) {
  return mat4(  mx, 0.0, 0.0, 0.0,
               0.0,  my, 0.0, 0.0,
               0.0, 0.0,  mz, 0.0,
               0.0, 0.0, 0.0, 1.0 );
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

mat4 glRotate( float angle, float x, float y, float z ) {
  float c = cos(angle);
  float s = sin(angle);
  return mat4( x*x*(1-c) + c, x*y*(1-c) - z*s, x*z*(1-c) + y*s, 0.0,
               y*x*(1-c) + z*s, y*y*(1-c) + c, y*z*(1-c) - x*s, 0.0,
               x*z*(1-c) - y*s, y*z*(1-c) + x*s, z*z*(1-c)+c, 0.0,
               0.0, 0.0, 0.0, 1.0);
}

mat4 glTranslate( float x, float y, float z ) {
  return mat4(1.0, 0.0, 0.0, x,
              0.0, 1.0, 0.0, y,
              0.0, 0.0, 1.0, z,
              0.0, 0.0, 0.0, 1.0);
}

float clampx ( float v ) { return floor(v) + 0.5; }
float clampy ( float v ) { return floor(v) + 0.5; }
vec4 ctexture( sampler2D tex, vec2 TexCoord ) {
  ivec2 TexCoord_uv = ivec2(clampx(TexCoord.x), clampy(TexCoord.y));
  return texelFetch(tex, TexCoord_uv, 0);
}
