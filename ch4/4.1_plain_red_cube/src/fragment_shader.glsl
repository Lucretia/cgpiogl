#version 460

in vec4 varyingColour;

out vec4 colour;

uniform mat4 mv_matrix;
uniform mat4 p_matrix;

void main(void) {
  colour = varyingColour;
}