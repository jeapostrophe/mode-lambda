@glsl-include["lib.glsl"]

uniform sampler2D LayerTargets[@LAYERS];
uniform sampler2D LayerConfigTex;
uniform float XScale;
uniform float YScale;
uniform vec2 LogicalSize;
uniform vec2 TextureSize;

in vec2 iTexCoord;

out vec4 oFragColor;

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
    //float ix = round(wx);
    return wx;
  }
}

void main() {
  float height = LogicalSize.y;
  float hheight = height / 2.0;
  float width = LogicalSize.x;
  float hwidth = width / 2.0;

  float ax = iTexCoord.x * width;
  float ay = height - iTexCoord.y * height;

  vec4 fin_Color = vec4(0.0, 0.0, 0.0, 1.0);
@in[compiletimelayer (in-range LAYERS)]{
  {
    int layer = @compiletimelayer;
    @glsl-include["layer.glsl"]
    float ay_horiz = horizon - ay;
    float pz = compute_pz(mode7coeff, ay_horiz);
    float ey = compute_e(ay, hheight, fov, pz, Lcy, Lhh);
    float ex = compute_e(ax, hwidth, fov, pz, Lcx, Lhw);
    vec4 lay_Color =
      texture(LayerTargets[@compiletimelayer],
              vec2((2.0 * (XScale * ex) + 1.0) / (2.0 * TextureSize.x),
                   (2.0 * (YScale * abs(ey - height) + 1.0)) / (2.0 * TextureSize.y)));
    if ((! (pz <= 0.0))
        && (0.0 <= ey && ey <= height)
        && (0.0 <= ex && ex <= width)) {
      fin_Color.rgb =
          fin_Color.rgb * (1.0 - lay_Color.a)
        + lay_Color.rgb * lay_Color.a;
    }
  }
}
  
  oFragColor = fin_Color;
}
