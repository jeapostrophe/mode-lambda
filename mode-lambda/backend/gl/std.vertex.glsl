#version 330

in vec4 iTexCoordPos;

uniform vec2 rubyInputSize;
uniform vec2 rubyTextureSize;
uniform vec2 rubyOutputSize;

out vec2 texCoord;

mat4 glOrtho( float left, float right, float bottom, float top, float nearVal, float farVal ) {
  float t_x = - (right + left) / (right - left);
  float t_y = - (top + bottom) / (top - bottom);
  float t_z = - (farVal + nearVal) / (farVal - nearVal);
  return mat4( 2.0 / right - left, 0.0, 0.0, t_x,
               0.0, 2.0 / top - bottom, 0.0, t_y,
               0.0, 0.0, -2 / farVal - nearVal, t_z,
               0.0, 0.0, 0.0, 1.0 );
}

void main() {
  vec2 iPos = iTexCoordPos.zw;
  vec2 iTexCoord = iTexCoordPos.xy;

  mat4 ViewportMatrix = glOrtho(0.0, rubyOutputSize.x,
                                0.0, rubyOutputSize.y,
                                1.0, -1.0);

  gl_Position = vec4(iPos.x, iPos.y, 0.0, 1.0) * ViewportMatrix;

  // Texture coords.
  texCoord = iTexCoord;	
}
