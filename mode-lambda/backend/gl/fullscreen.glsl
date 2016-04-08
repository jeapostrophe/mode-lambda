#define VERTS @FULLSCREEN_VERTS

const vec2 coordData[VERTS] =
  vec2[VERTS]( vec2(0.0, 1.0),
               vec2(0.0, 0.0),
               vec2(1.0, 1.0),               
               vec2(1.0, 0.0) );

vec2 compute_iTexCoord() {
  return coordData[ gl_VertexID ];
}
