#version 330

uniform sampler2D rubyTexture;
uniform vec2 rubyInputSize;
uniform vec2 rubyTextureSize;

in vec2 texCoord;

out vec4 oFragColor;

void main() {
  // Doesn't work
  // oFragColor = texelFetch(rubyTexture, ivec2(texCoord), 0);
  // Looks okay, but I feel like I want to use texelFetch
  oFragColor = texture(rubyTexture, texCoord);
}
