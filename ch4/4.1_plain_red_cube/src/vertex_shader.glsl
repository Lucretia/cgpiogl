#version 460

layout (location=0) in vec3 position;

uniform mat4 mv_matrix;
uniform mat4 m_matrix;
uniform mat4 v_matrix;
uniform mat4 p_matrix;

void main(void) {
  mv_matrix = v_matrix * m_matrix;
  gl_Position = p_matrix * mv_matrix * vec4(position, 1.0);
}

