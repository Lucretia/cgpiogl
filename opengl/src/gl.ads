with Ada.Finalization;
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;

package GL is
   package US renames Ada.Strings.Unbounded;
   package C renames Interfaces.C;

   type Float32 is new C.C_float;   --  GLfloat
   type UInt    is new C.unsigned;  --  GLuint
   type SizeI   is new C.int;       --  GLsizei
   type Int     is new C.int;       --  GLint

   type Int_Array is array (C.size_t range <>) of Int with
     Convention => C;
   type UInt_Array is array (C.size_t range <>) of UInt with
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

   type Shader_Program_Array is array (C.size_t range <>) of US.Unbounded_String;

   type Shader_Programs (Length : C.size_t) is new Ada.Finalization.Controlled with record
      Program        : C.Strings.chars_ptr_array (1 .. Length);
      String_Lengths : Int_Array (1 .. Length);
   end record;

   overriding
   procedure Finalize (Self : in out Shader_Programs);

   function Convert (Shader_Program : Shader_Program_Array) return Shader_Programs;

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
   type Create_Program_Ptr is access function return UInt with
     Convention => C;

   Create_Program : Create_Program_Ptr := null;

   --  typedef void (APIENTRYP PFNGLATTACHSHADERPROC) (GLuint program, GLuint shader);
   --  GLAPI void APIENTRY glAttachShader (GLuint program, GLuint shader);
   type Attach_Shader_Ptr is access procedure (Program, Shader : UInt) with
     Convention => C;

   Attach_Shader : Attach_Shader_Ptr := null;

   --  typedef void (APIENTRYP PFNGLLINKPROGRAMPROC) (GLuint program);
   --  GLAPI void APIENTRY glLinkProgram (GLuint program);
   type Link_Program_Ptr is access procedure (Program : UInt) with
     Convention => C;

   Link_Program : Link_Program_Ptr := null;

   --  typedef void (APIENTRYP PFNGLGENVERTEXARRAYSPROC) (GLsizei n, GLuint *arrays);
   --  GLAPI void APIENTRY glGenVertexArrays (GLsizei n, GLuint *arrays);
   type Gen_Vertex_Arrays_Ptr is access procedure (N : GL.SizeI; Arrays : in out UInt_Array) with
     Convention => C;

   Gen_Vertex_Arrays : Gen_Vertex_Arrays_Ptr := null;

   --  typedef void (APIENTRYP PFNGLBINDVERTEXARRAYPROC) (GLuint array);
   --  GLAPI void APIENTRY glBindVertexArray (GLuint array);
   type Bind_Vertex_Array_Ptr is access procedure (Arr : UInt) with
     Convention => C;

   Bind_Vertex_Array : Bind_Vertex_Array_Ptr := null;

   --  typedef void (APIENTRYP PFNGLUSEPROGRAMPROC) (GLuint program);
   --  GLAPI void APIENTRY glUseProgram (GLuint program);
   type Use_Program_Ptr is access procedure (Program : UInt) with
     Convention => C;

   Use_Program : Use_Program_Ptr := null;

   --  #define GL_POINTS                         0x0000
   --  #define GL_LINES                          0x0001
   --  #define GL_LINE_LOOP                      0x0002
   --  #define GL_LINE_STRIP                     0x0003
   --  #define GL_TRIANGLES                      0x0004
   --  #define GL_TRIANGLE_STRIP                 0x0005
   --  #define GL_TRIANGLE_FAN                   0x0006
   --  #define GL_LINES_ADJACENCY                0x000A
   --  #define GL_LINE_STRIP_ADJACENCY           0x000B
   --  #define GL_TRIANGLES_ADJACENCY            0x000C
   --  #define GL_TRIANGLE_STRIP_ADJACENCY       0x000D
   --  #define GL_PATCHES                        0x000E
   type Primitive_Modes is
     (Points,
      Lines,
      Line_Loop,
      Line_Strip,
      Triangles,
      Triangle_Strip,
      Triangle_Fan,
      Lines_Adjacency,
      Line_Strip_Adjacency,
      Triangles_Adjacency,
      Triangle_Strip_Adjacency,
      Patches);

   for Primitive_Modes use
     (Points                   => 16#0000#,
      Lines                    => 16#0001#,
      Line_Loop                => 16#0002#,
      Line_Strip               => 16#0003#,
      Triangles                => 16#0004#,
      Triangle_Strip           => 16#0005#,
      Triangle_Fan             => 16#0006#,
      Lines_Adjacency          => 16#000A#,
      Line_Strip_Adjacency     => 16#000B#,
      Triangles_Adjacency      => 16#000C#,
      Triangle_Strip_Adjacency => 16#000D#,
      Patches                  => 16#000E#);

   --  typedef void (APIENTRYP PFNGLDRAWARRAYSPROC) (GLenum mode, GLint first, GLsizei count);
   --  GLAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
   TYPE Draw_Arrays_Ptr is access procedure (Mode : Primitive_Modes; First : Int; Count : SizeI) with
     Convention => C;

   Draw_Arrays : Draw_Arrays_Ptr := null;
end GL;
