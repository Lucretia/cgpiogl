#version 460

out vec4 colour;

uniform mat4 mv_matrix;
uniform mat4 p_matrix;

void main(void) {
  colour = vec4(1.0, 0.0, 0.0, 1.0);
}