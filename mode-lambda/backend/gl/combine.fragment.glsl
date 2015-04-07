#version 330

uniform sampler2D LayerTargets[@LAYERS];

in vec2 texCoord;

out vec4 oFragColor;

void main() {
  vec4 fin_Color = vec4(0.0, 0.0, 0.0, 1.0);

  for (int i = 0 ; i < @LAYERS ; i++) {
    // xxx implement the mode7 formula (computes a new texCoord)
    vec4 lay_Color = texture(LayerTargets[i], texCoord);
    fin_Color.rgb = fin_Color.rgb * (1.0 - lay_Color.a) + lay_Color.rgb * lay_Color.a;
  }
  
  oFragColor = fin_Color;
}
