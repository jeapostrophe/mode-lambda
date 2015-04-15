#version 330 core
@glsl-include["lib.glsl"]
@glsl-include["fullscreen.glsl"]

uniform sampler2D LayerTargets[@LAYERS];
uniform sampler2D LayerConfigTex;
uniform vec2 LogicalSize;

out vec2 iTexCoord;

void main() {
  iTexCoord = compute_iTexCoord();
  
  gl_Position =
    vec4( iTexCoord.x * LogicalSize.x, iTexCoord.y * LogicalSize.y, 0.0, 1.0)
    * glOrtho(0.0, LogicalSize.x,
              0.0, LogicalSize.y,
              1.0, -1.0);
}
