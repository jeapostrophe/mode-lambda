#version 330 core
@glsl-include["lib.glsl"]
@glsl-include["fullscreen.glsl"]

uniform sampler2D LayerTargets[@LAYERS];
uniform sampler2D LayerConfigTex;
uniform vec2 LogicalSize;

out vec2 texCoord;
flat out vec4 L_CY_HH_CX_HW[@LAYERS];
flat out vec3 L_M7_H_F[@LAYERS];

void main() {
  vec2 iTexCoord = compute_iTexCoord();

  gl_Position =
    vec4( iTexCoord.x * LogicalSize.x, iTexCoord.y * LogicalSize.y, 0.0, 1.0)
    * glOrtho(0.0, LogicalSize.x,
              0.0, LogicalSize.y,
              1.0, -1.0);

  float width = LogicalSize.x;
  float height = LogicalSize.y;
  float hwidth = width / 2.0;
  float hheight = height / 2.0;
  float ax = iTexCoord.x * width;
  float ay = height - iTexCoord.y * height;

  texCoord = vec2(ax,ay);

  for (int layer = 0 ; layer < @LAYERS ; layer++) {
    @glsl-include["layer.glsl"]
    L_CY_HH_CX_HW[layer] = vec4(Lcy, Lhh, Lcx, Lhw);
    L_M7_H_F[layer] = vec3(mode7coeff, horizon, fov);
  }
}
