/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,
   see https://castle-engine.sourceforge.io/compositing_shaders.php ),
   applied over terrain.

   This simply saves position, to be used by terrain.fs code. */

varying vec3 position;
void PLUG_vertex_object_space(
  const in vec4 vertex_object, const in vec3 normal_object)
{
  position = vec3(vertex_object);
}
