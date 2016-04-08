#define VERTS @QUAD_VERTS

const vec2 full_coordData[VERTS] =
  vec2[VERTS]( vec2(0.0, 1.0),
               vec2(0.0, 0.0),
               vec2(1.0, 1.0),               
               vec2(1.0, 0.0) );

const vec2 center_coordData[VERTS] =
  vec2[VERTS]( vec2(-1.0, +1.0),
               vec2(-1.0, -1.0),
               vec2(+1.0, +1.0),               
               vec2(+1.0, -1.0) );

vec2 compute_full_iTexCoord() {
  return full_coordData[ gl_VertexID ];
}

vec2 compute_center_iTexCoord() {
  return center_coordData[ gl_VertexID ];
}
