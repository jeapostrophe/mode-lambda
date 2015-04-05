uniform vec2 rubyInputSize;
uniform vec2 rubyOutputSize;

float compute_scale( float big, float small ) {
  // This can compute values like 1.6789321
  //
  // In Racket, I had a function to ensure that the result was always
  //    = Nat + Sum( c_i * 2^{-i} )
  // for some c_i
  return (big / small);
}

#define VERTS @EFFECT_VERTS

const vec2 coordData[VERTS] =
  vec2[VERTS]( vec2(0.0, 0.0),
               vec2(1.0, 0.0),
               vec2(1.0, 1.0),

               vec2(0.0, 1.0),
               vec2(1.0, 1.0),
               vec2(0.0, 0.0) );

vec2 compute_iTexCoord() {
  return coordData[ gl_VertexID ];
}

vec2 compute_iPos( vec2 iTexCoord ) {
  float actual_screen_width = rubyOutputSize.x;
  float actual_screen_height = rubyOutputSize.y;
  float crt_width = rubyInputSize.x;
  float crt_height = rubyInputSize.y;

  float scale = min(compute_scale(actual_screen_width, crt_width),
                    compute_scale(actual_screen_height, crt_height));

  float screen_width = scale * crt_width;
  float screen_height = scale * crt_height;

  float inset_left = (actual_screen_width - screen_width) / 2.0;
  float inset_right = inset_left + screen_width;
  float inset_bot = (actual_screen_height - screen_height) / 2.0;
  float inset_top = inset_bot + screen_height;

  return vec2( iTexCoord.x == 0.0 ? inset_left : inset_right,
               iTexCoord.y == 0.0 ? inset_bot : inset_top );
}
