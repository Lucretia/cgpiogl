with Interfaces.C.Strings;

package GL is
   package C renames Interfaces.C;

   type Float32 is new C.C_float;   --  GLfloat
   type UInt    is new C.unsigned;  --  GLuint
   type SizeI   is new C.int;       --  GLsizei
   type Int     is new C.int;       --  GLint

   type Int_Array is array (Natural range <>) of Int with
     Convention => C;

   --  typedef void (APIENTRYP PFNGLCLEARCOLORPROC) (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
   --  GLAPI void APIENTRY glClearColor (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
   type Clear_Colour_Ptr is access procedure (Red, Green, Blue, Alpha : Float32) with
     Convention => C;

   Clear_Colour : Clear_Colour_Ptr := null;

   --  #define GL_DEPTH_BUFFER_BIT               0x00000100
   --  #define GL_STENCIL_BUFFER_BIT             0x00000400
   --  #define GL_COLOR_BUFFER_BIT               0x00004000
   type Clear_Buffer_Mask is new C.int;

   Depth_Buffer_Bit   : constant Clear_Buffer_Mask := 16#0000_0100#;
   Stencil_Buffer_Bit : constant Clear_Buffer_Mask := 16#0000_0400#;
   Color_Buffer_Bit   : constant Clear_Buffer_Mask := 16#0000_4000#;

   --  typedef void (APIENTRYP PFNGLCLEARPROC) (GLbitfield mask);
   --  GLAPI void APIENTRY glClear (GLbitfield mask);
   type Clear_Ptr is access procedure (Mask : Clear_Buffer_Mask) with
     Convention => C;

   Clear : Clear_Ptr := null;

   --  #define GL_COMPUTE_SHADER                 0x91B9
   --  #define GL_VERTEX_SHADER                  0x8B31
   --  #define GL_TESS_CONTROL_SHADER            0x8E88
   --  #define GL_TESS_EVALUATION_SHADER         0x8E87
   --  #define GL_GEOMETRY_SHADER                0x8DD9
   --  #define GL_FRAGMENT_SHADER                0x8B30
   type Shader_Types is
     (Fragment,
      Vertex,
      Geometry,
      Tess_Evaluation,
      Tess_Control,
      Compute) with
     Convention => C;

   for Shader_Types use
     (Fragment        => 16#8B30#,
      Vertex          => 16#8B31#,
      Geometry        => 16#8DD9#,
      Tess_Evaluation => 16#8E87#,
      Tess_Control    => 16#8E88#,
      Compute         => 16#91B9#);

   --  typedef GLuint (APIENTRYP PFNGLCREATESHADERPROC) (GLenum type);
   --  GLAPI GLuint APIENTRY glCreateShader (GLenum type);
   type Create_Shader_Ptr is access function (Shader_Type : Shader_Types) return UInt with
     Convention => C;

   Create_Shader : Create_Shader_Ptr := null;

   --  typedef void (APIENTRYP PFNGLSHADERSOURCEPROC) (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
   --  GLAPI void APIENTRY glShaderSource (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
   type Shader_Source_Ptr is access procedure
     (Shader  : UInt;
      Count   : SizeI;
      Program : C.Strings.chars_ptr_array;
      Length  : Int_Array) with
     Convention => C;

   Shader_Source : Shader_Source_Ptr := null;

   --  typedef void (APIENTRYP PFNGLCOMPILESHADERPROC) (GLuint shader);
   --  GLAPI void APIENTRY glCompileShader (GLuint shader);
   type Compile_Shader_Ptr is access procedure (Shader : UInt) with
     Convention => C;

   Compile_Shader : Compile_Shader_Ptr := null;

   --  typedef GLuint (APIENTRYP PFNGLCREATEPROGRAMPROC) (void);
   --  GLAPI GLuint APIENTRY glCreateProgram (void);
   type Create_Program_Ptr is access procedure with
     Convention => C;

   Create_Program : Create_Program_Ptr := null;
end GL;
