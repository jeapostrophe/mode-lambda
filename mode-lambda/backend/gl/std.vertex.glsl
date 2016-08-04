@glsl-include["lib.glsl"]
@glsl-include["effect-api.glsl"]

out vec2 texCoord;

void main() {
  vec2 iTexCoord = compute_full_iTexCoord();

  gl_Position =
      vec4(compute_iPos( iTexCoord ), 0.0, 1.0)
    * glOrtho(0.0, ScreenSize.x,
              0.0, ScreenSize.y,
              1.0, -1.0);

  texCoord = iTexCoord;	
}
