#version 330

uniform sampler2D LayerTargets[@LAYERS];

in vec2 texCoord;

out vec4 oFragColor;

void main() {
  // xxx implement the correct formula
  oFragColor = texture(LayerTargets[0], texCoord);
}
