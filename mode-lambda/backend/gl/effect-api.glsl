uniform vec2 LogicalSize;
uniform vec2 ScreenSize;
uniform vec2 TextureSize;
uniform float XScale;
uniform float YScale;

@glsl-include["fullscreen.glsl"]

vec2 compute_iPos( vec2 iTexCoord ) {
  float actual_screen_width = ScreenSize.x;
  float actual_screen_height = ScreenSize.y;
  float crt_width = LogicalSize.x;
  float crt_height = LogicalSize.y;

  float screen_width = XScale * crt_width;
  float screen_height = YScale * crt_height;

  float inset_left = (actual_screen_width - screen_width) / 2.0;
  float inset_right = inset_left + screen_width;
  float inset_bot = (actual_screen_height - screen_height) / 2.0;
  float inset_top = inset_bot + screen_height;

  return vec2( iTexCoord.x == 0.0 ? inset_left : inset_right,
               iTexCoord.y == 0.0 ? inset_bot : inset_top );
}
