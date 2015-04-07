#version 330 core
@glsl-include["lib.glsl"]
@glsl-include["fullscreen.glsl"]

uniform sampler2D LayerTargets[@LAYERS];
uniform sampler2D LayerConfigTex;
uniform uint ViewportWidth;
uniform uint ViewportHeight;

out vec2 texCoord;

void main() {
  vec2 iTexCoord = compute_iTexCoord();

  gl_Position =
      vec4( iTexCoord.x * ViewportWidth, iTexCoord.y * ViewportHeight, 0.0, 1.0)
    * glOrtho(0.0, ViewportWidth,
              0.0, ViewportHeight,
              1.0, -1.0);

  texCoord = iTexCoord;	
}
