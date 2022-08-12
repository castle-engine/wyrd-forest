varying vec4 castle_TexCoord0;

uniform vec2 hitPoint;
uniform vec3 clipPlaneAngles;
uniform int part;

void PLUG_main_texture_apply(inout vec4 fragment_color, const in vec3 normal_eye)
{
  vec2 xy = castle_TexCoord0.xy - hitPoint;
  float a = atan(xy.y, xy.x);

  // debug a value
  /* fragment_color.xyz = vec3((a + 3.14) / (2.0 * 3.14)); */

  if (part == 0) {
    if (a > clipPlaneAngles.x && a < clipPlaneAngles.z) { discard; }
  }
  if (part == 1) {
    if (a < clipPlaneAngles.x || a > clipPlaneAngles.y) { discard; }
  }
  if (part == 2) {
    if (a < clipPlaneAngles.y || a > clipPlaneAngles.z) { discard; }
  }
}
