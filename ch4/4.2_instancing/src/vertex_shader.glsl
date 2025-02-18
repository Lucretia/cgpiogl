#version 460

layout (location=0) in vec3 position;

uniform mat4 v_matrix;
uniform mat4 p_matrix;
uniform float tf;      // time factor for animation and placement of cubes.

out vec4 varyingColour;

mat4 buildRotateX(float rad);
mat4 buildRotateY(float rad);
mat4 buildRotateZ(float rad);
mat4 buildTranslate(float x, float y, float z);

void main(void) {
  float i = gl_InstanceID + tf;   // value based on time factor, but different for each cube instance.
  float a = sin (2.0 * i) * 8.0;  // these are the x, y, and z components for the translation, below.
  float b = sin (3.0 * i) * 8.0;
  float c = sin (4.0 * i) * 8.0;

  // build the rotation and translation matrices to be applied to this cube’s model matrix.
  mat4 localRotX = buildRotateX (1000 * i);
  mat4 localRotY = buildRotateY (1000 * i);
  mat4 localRotZ = buildRotateZ (1000 * i);
  mat4 localTrans = buildTranslate (a, b, c);

  // build the model matrix and then the model-view matrix.
  mat4 newModel  = localTrans * localRotX * localRotY * localRotZ;
  mat4 mv_matrix = v_matrix * newModel;

  gl_Position   = p_matrix * mv_matrix * vec4 (position, 1.0);
  varyingColour = vec4(position, 1.0) * 0.5 + vec4(0.5, 0.5, 0.5, 0.5);
}

// builds and returns a matrix that performs a rotation around the X axis
mat4 buildRotateX(float rad)
{
  mat4 xrot = mat4(
    1.0, 0.0,       0.0, 0.0,
		0.0, cos(rad), -sin(rad), 0.0,
		0.0, sin(rad),  cos(rad), 0.0,
		0.0, 0.0,       0.0, 1.0 );
  return xrot;
}

// builds and returns a matrix that performs a rotation around the Y axis
mat4 buildRotateY(float rad)
{
  mat4 yrot = mat4(
     cos(rad), 0.0, sin(rad), 0.0,
		 0.0,      1.0, 0.0,      0.0,
		-sin(rad), 0.0, cos(rad), 0.0,
		 0.0,      0.0, 0.0,      1.0 );

  return yrot;
}

// builds and returns a matrix that performs a rotation around the Z axis
mat4 buildRotateZ(float rad)
{
  mat4 zrot = mat4(
    cos(rad), -sin(rad), 0.0, 0.0,
		sin(rad),  cos(rad), 0.0, 0.0,
		0.0,       0.0,      1.0, 0.0,
		0.0,       0.0,      0.0, 1.0 );

  return zrot;
}

// utility function to build a translation matrix (from Chapter 3)
mat4 buildTranslate(float x, float y, float z)
{
  mat4 trans = mat4(
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    x,   y,   z,   1.0
  );

  return trans;
}
