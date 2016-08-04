@glsl-include["lib.glsl"]

uniform sampler2D CombinedTex;

in vec2 texCoord;

out vec4 oFragColor;

void main() {
  oFragColor = texture(CombinedTex, texCoord);
}
