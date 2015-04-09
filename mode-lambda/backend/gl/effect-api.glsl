uniform vec2 rubyInputSize;
uniform vec2 rubyOutputSize;
uniform float scale;

@glsl-include["fullscreen.glsl"]

vec2 compute_iPos( vec2 iTexCoord ) {
  float actual_screen_width = rubyOutputSize.x;
  float actual_screen_height = rubyOutputSize.y;
  float crt_width = rubyInputSize.x;
  float crt_height = rubyInputSize.y;

  float screen_width = scale * crt_width;
  float screen_height = scale * crt_height;

  float inset_left = (actual_screen_width - screen_width) / 2.0;
  float inset_right = inset_left + screen_width;
  float inset_bot = (actual_screen_height - screen_height) / 2.0;
  float inset_top = inset_bot + screen_height;

  return vec2( iTexCoord.x == 0.0 ? inset_left : inset_right,
               iTexCoord.y == 0.0 ? inset_bot : inset_top );
}
