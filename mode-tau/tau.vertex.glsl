#version 410
@glsl-include["../mode-lambda/backend/gl/lib.glsl"]

uniform sampler2D SpriteAtlasTex; 
uniform sampler2D SpriteIndexTex;

@cstruct-info->glsl-in[_glyph-data:info]

uniform vec2 Viewport;

out vec3 FGColor;
out vec3 BGColor;
out vec2 TexCoord;

void main(void)
{
  vec4 in_TexCoord =
    texelFetch(SpriteIndexTex, ivec2(0, glyph), 0);

  float  w = in_TexCoord.x;
  float  h = in_TexCoord.y;
  float tx = in_TexCoord.z;
  float ty = in_TexCoord.w;

  gl_Position =
      vec4(horiz, vert, 0.0, 1.0)
    * glScale(w * 0.5, h * 0.5, 1.0 )
    * glTranslate(dx, dy, 0.0)
    * glOrtho(0.0, Viewport.x,
              0.0, Viewport.y,
              1.0, -1.0)
    * glScale(1.0, -1.0, 1.0);

  ivec2 atlasSize = textureSize(SpriteAtlasTex, 0);
  
  TexCoord =
    vec2(tx + ((horiz + 1.0)/+2.0) * w,
         ty + (( vert + 1.0)/+2.0) * h)
    / float(atlasSize.x);

  FGColor = vec3(fgr, fgg, fgb) / 255.0;
  BGColor = vec3(bgr, bgg, bgb) / 255.0;
}
