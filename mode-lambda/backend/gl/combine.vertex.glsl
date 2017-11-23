@glsl-include["lib.glsl"]
@glsl-include["quad.glsl"]

uniform sampler2D LayerConfigTex;
uniform sampler2DArray LayerTargets;
uniform float XScale;
uniform float YScale;
uniform vec2 LogicalSize;
uniform vec2 TextureSize;

out vec2 iTexCoord;

void main() {
  iTexCoord = compute_full_iTexCoord();
  
  gl_Position =
    vec4( iTexCoord.x * LogicalSize.x, iTexCoord.y * LogicalSize.y, 0.0, 1.0)
    * glOrtho(0.0, LogicalSize.x,
              0.0, LogicalSize.y,
              1.0, -1.0);
}
