#version 410
@glsl-include["../mode-lambda/backend/gl/lib.glsl"]

uniform sampler2D SpriteAtlasTex; 

in vec3 FGColor;
in vec3 BGColor;
in vec2 TexCoord;

out vec4 out_Color;

void main(void)
{
  vec4 SpriteColor = texture(SpriteAtlasTex, TexCoord);

  // Black = FGColor
  // White = BGColor
  // lerp between them
  
  out_Color = vec4(mix(FGColor.r, BGColor.r, SpriteColor.r),
                   mix(FGColor.g, BGColor.g, SpriteColor.g),
                   mix(FGColor.b, BGColor.b, SpriteColor.b),
                   1.0);
}
