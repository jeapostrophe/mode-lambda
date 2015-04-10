#version 330
@glsl-include["lib.glsl"]

uniform sampler2D LayerTargets[@LAYERS];
uniform float Scale;
uniform vec2 LogicalSize;

in vec2 texCoord;
flat in vec4 L_CY_HH_CX_HW[@LAYERS];
flat in vec3 L_M7_H_F[@LAYERS];

out vec4 oFragColor;

float jmod(float a, float n) {
  return a - (n * floor(a / n));
}

float wrap(float v, float c, float hrng) {
  if (isinf(hrng)) {
    return v;
  } else {
    return (c + ((jmod(v, (2.0 * hrng))) - hrng));
  }
}

float compute_pz(float mode7coeff, float ay_horiz) {
  if (mode7coeff == 0.0) { return 1.0; }
  if (mode7coeff == 1.0) { return +1.0 * ay_horiz; }
  if (mode7coeff == 2.0) { return -1.0 * ay_horiz; }
  if (mode7coeff == 3.0) { return abs(ay_horiz); }
  return 0.0;
}

float compute_e(float ax, float hwidth, float fov, float pz, float Lcx, float Lhw) {
  // Y' = ((Y - Yc) * (F/Z)) + Yc
  float rx = ((ax - hwidth) * fov / pz) + hwidth;
  if (isinf(rx) || isnan(rx)) {
    return -1.0;
  } else {
    float wx = wrap(rx, Lcx, Lhw);
    float ix = round(wx);
    return ix;
  }
}

void main() {
  float ax = texCoord.x;
  float ay = texCoord.y;
  
  float height = LogicalSize.y;
  float hheight = height / 2.0;
  float width = LogicalSize.x;
  float hwidth = width / 2.0;

  vec4 fin_Color = vec4(0.0, 0.0, 0.0, 1.0);
  for (int layer = 0 ; layer < @LAYERS ; layer++) {
    float Lcy = L_CY_HH_CX_HW[layer].x;
    float Lhh = L_CY_HH_CX_HW[layer].y;
    float Lcx = L_CY_HH_CX_HW[layer].z;
    float Lhw = L_CY_HH_CX_HW[layer].w;
    float mode7coeff = L_M7_H_F[layer].x;
    float horizon = L_M7_H_F[layer].y;
    float fov = L_M7_H_F[layer].z;
    
    float ay_horiz = horizon - ay;
    float pz = compute_pz(mode7coeff, ay_horiz);
    float ey = compute_e(ay, hheight, fov, pz, Lcy, Lhh);
    float ex = compute_e(ax, hwidth, fov, pz, Lcx, Lhw);
    vec4 lay_Color = ctexture(LayerTargets[layer],
                              Scale * vec2(ex, (abs(ey - height))));
    if ((! (pz <= 0.0))
        && (0.0 <= ey && ey <= height)
        && (0.0 <= ex && ex <= width)) {
      fin_Color.rgb =
          fin_Color.rgb * (1.0 - lay_Color.a)
        + lay_Color.rgb * lay_Color.a;
    }
  }
  
  oFragColor = fin_Color;
}
