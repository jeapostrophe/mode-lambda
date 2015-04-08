// except for inside functions

vec4 LC_CX_CY_HW_HH =
  texelFetch(LayerConfigTex, ivec2(0, layer), 0);
float Lcx = LC_CX_CY_HW_HH.x;
float Lcy = LC_CX_CY_HW_HH.y;
float Lhw = LC_CX_CY_HW_HH.z;
float Lhh = LC_CX_CY_HW_HH.w;
float Lw = 2 * Lhw;
float Lh = 2 * Lhh;
vec4 LC_MX_MY_THETA_MODE7COEEF =
  texelFetch(LayerConfigTex, ivec2(1, layer), 0);
float Lmx = LC_MX_MY_THETA_MODE7COEEF.x;
float Lmy = LC_MX_MY_THETA_MODE7COEEF.y;
float Ltheta = LC_MX_MY_THETA_MODE7COEEF.z;
float mode7coeff = LC_MX_MY_THETA_MODE7COEEF.w;
vec4 LC_HORIZON_FOV_WRAPXP_WRAPYP =
  texelFetch(LayerConfigTex, ivec2(2, layer), 0);
float horizon = LC_HORIZON_FOV_WRAPXP_WRAPYP.x;
float fov = LC_HORIZON_FOV_WRAPXP_WRAPYP.y;
float wrapxp = LC_HORIZON_FOV_WRAPXP_WRAPYP.z;
float wrapyp = LC_HORIZON_FOV_WRAPXP_WRAPYP.w;
