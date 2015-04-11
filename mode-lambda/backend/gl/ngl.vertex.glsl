#version 410
@glsl-include["lib.glsl"]

uniform sampler2D SpriteIndexTex;

@cstruct-info->glsl-in[_sprite-data:info]

uniform vec2 LogicalSize;

uniform sampler2D LayerConfigTex;

out vec4 Color;
out vec2 TexCoord;
out float Palette;
out float Layer;

void main(void)
{
  @glsl-include["layer.glsl"]
  // xxx use wrapxp and wrapyp -- the software renderer uses these to
  // decide whether to mess with the points. i really need to generate
  // 4 times more vertices and add two more parameters that are
  // bools. it's not normally possible to "discard" a vertex, but i
  // could set the alpha to 0 or i could set w to 0, so it is
  // projected out to infinity.
  //
  // Another way is to set the dimensions of wrapped layer textures to
  // be different than everything else and wrapped...? But I don't
  // think that will work because the stuff "offscreen" out of the
  // view port will never be drawn so it won't even go into the
  // texture to be repeated.

  vec4 in_TexCoord =
    texelFetch(SpriteIndexTex, ivec2(0, spr), 0);

  float  w = in_TexCoord.x;
  float  h = in_TexCoord.y;
  float tx = in_TexCoord.z;
  float ty = in_TexCoord.w;

  Color = vec4(r / 255.0, g / 255.0, b / 255.0, a);
  
  gl_Position =
      vec4(horiz * w * 0.5 * mx * Lmx, vert * h * 0.5 * my * Lmy, 0.0, 1.0)
    * glRotate(theta, 0.0, 0.0, 1.0)
    * glTranslate(dx, dy, 0.0)
    // xxx These might be Lhw and Lhh
    * glTranslate(-1.0 * LogicalSize.x / 2.0, -1.0 * LogicalSize.y / 2.0, 0.0)
    * glRotate(Ltheta, 0.0, 0.0, 1.0)
    * glTranslate(Lcx, Lcy, 0.0)
    * glOrtho(0.0, LogicalSize.x,
              0.0, LogicalSize.y,
              1.0, -1.0);
  TexCoord =
    vec2(tx + ((horiz + 1.0)/+2.0) * w,
         ty + (( vert - 1.0)/-2.0) * h);
  Palette = pal;
  Layer = layer;
}
