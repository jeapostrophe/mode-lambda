#version 330
@glsl-include["lib.glsl"]

uniform sampler2D rubyTexture;
uniform vec2 rubyInputSize;

in vec2 texCoord;

out vec4 oFragColor;

void main() {
  // xxx use ctexture?
  oFragColor = texture(rubyTexture, texCoord);
}
