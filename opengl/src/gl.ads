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
   type Clear_Buffer_Mask is mod C.int'Last;

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
      Patches) with
     Convention => C;

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
   type Draw_Arrays_Ptr is access procedure (Mode : Primitive_Modes; First : Int; Count : SizeI) with
     Convention => C;

   Draw_Arrays : Draw_Arrays_Ptr := null;

   --  typedef void (APIENTRYP PFNGLPOINTSIZEPROC) (GLfloat size);
   --  GLAPI void APIENTRY glPointSize (GLfloat size);
   type Point_Size_Ptr is access procedure (Size : Float32) with
     Convention => C;

   Point_Size : Point_Size_Ptr := null;

   --  #define GL_NO_ERROR                       0
   --  #define GL_INVALID_ENUM                   0x0500
   --  #define GL_INVALID_VALUE                  0x0501
   --  #define GL_INVALID_OPERATION              0x0502
   --  #define GL_STACK_OVERFLOW                 0x0503
   --  #define GL_STACK_UNDERFLOW                0x0504
   --  #define GL_OUT_OF_MEMORY                  0x0505
   --  #define GL_INVALID_FRAMEBUFFER_OPERATION  0x0506
   type Errors is
     (No_Error,
      Invalid_Enum,
      Invalid_Value,
      Invalid_Operation,
      Stack_Overflow,
      Stack_Underflow,
      Out_Of_Memory,
      Invalid_Framebuffer_Operation) with
     Convention => C;

   for Errors use
     (No_Error                      => 0,
      Invalid_Enum                  => 16#0500#,
      Invalid_Value                 => 16#0501#,
      Invalid_Operation             => 16#0502#,
      Stack_Overflow                => 16#0503#,
      Stack_Underflow               => 16#0504#,
      Out_Of_Memory                 => 16#0505#,
      Invalid_Framebuffer_Operation => 16#0506#);

   --  typedef GLenum (APIENTRYP PFNGLGETERRORPROC) (void);
   --  GLAPI GLenum APIENTRY glGetError (void);
   type Get_Error_Ptr is access function return Errors with
     Convention => C;

   Get_Error : Get_Error_Ptr := null;

--  #define GL_LINE_SMOOTH                    0x0B20
--  #define GL_POLYGON_SMOOTH                 0x0B41
--  #define GL_CULL_FACE                      0x0B44
--  #define GL_DEPTH_TEST                     0x0B71
--  #define GL_STENCIL_TEST                   0x0B90
--  #define GL_DITHER                         0x0BD0
--  #define GL_BLEND                          0x0BE2
--  #define GL_COLOR_LOGIC_OP                 0x0BF2
--  #define GL_SCISSOR_TEST                   0x0C11
--  #define GL_POLYGON_OFFSET_POINT           0x2A01
--  #define GL_POLYGON_OFFSET_LINE            0x2A02
--  #define GL_CLIP_DISTANCE0                 0x3000
--  #define GL_CLIP_DISTANCE1                 0x3001
--  #define GL_CLIP_DISTANCE2                 0x3002
--  #define GL_CLIP_DISTANCE3                 0x3003
--  #define GL_CLIP_DISTANCE4                 0x3004
--  #define GL_CLIP_DISTANCE5                 0x3005
--  #define GL_CLIP_DISTANCE6                 0x3006
--  #define GL_CLIP_DISTANCE7                 0x3007
--  #define GL_POLYGON_OFFSET_FILL            0x8037
--  #define GL_MULTISAMPLE                    0x809D
--  #define GL_SAMPLE_ALPHA_TO_COVERAGE       0x809E
--  #define GL_SAMPLE_ALPHA_TO_ONE            0x809F
--  #define GL_SAMPLE_COVERAGE                0x80A0
--  #define GL_DEBUG_OUTPUT_SYNCHRONOUS       0x8242
--  #define GL_PROGRAM_POINT_SIZE             0x8642
--  #define GL_DEPTH_CLAMP                    0x864F
--  #define GL_TEXTURE_CUBE_MAP_SEAMLESS      0x884F
--  #define GL_SAMPLE_SHADING                 0x8C36
--  #define GL_RASTERIZER_DISCARD             0x8C89
--  #define GL_PRIMITIVE_RESTART_FIXED_INDEX  0x8D69
--  #define GL_FRAMEBUFFER_SRGB               0x8DB9
--  #define GL_SAMPLE_MASK                    0x8E51
--  #define GL_PRIMITIVE_RESTART              0x8F9D
--  #define GL_DEBUG_OUTPUT                   0x92E0

   --  TODO: Complete this.
   type Capabilities is
     (Line_Smooth,
      Polygon_Smooth,
      Cull_Face,
      Depth_Test,
      Stencil_Test,
      Dither,
      Blend,
      Color_Logic_Op,
      Scissor_Test,
      Polygon_Offset_Point,
      Polygon_Offset_Line,
      Clip_Distance0,
      Clip_Distance1,
      Clip_Distance2,
      Clip_Distance3,
      Clip_Distance4,
      Clip_Distance5,
      Clip_Distance6,
      Clip_Distance7,
      Polygon_Offset_Fill,
      Multisample,
      Sample_Alpha_To_Coverage,
      Sample_Alpha_To_One,
      Sample_Coverage,
      Debug_Output_Synchronous,
      Program_Point_Size,
      Depth_Clamp,
      Texture_Cube_Map_Seamless,
      Sample_Shading,
      Rasterizer_Discard,
      Primitive_Restart_Fixed_Index,
      Framebuffer_Srgb,
      Sample_Mask,
      Primitive_Restart,
      Debug_Output) with
     Convention => C;

   for Capabilities use
     (Line_Smooth                   => 16#0B20#,
      Polygon_Smooth                => 16#0B41#,
      Cull_Face                     => 16#0B44#,
      Depth_Test                    => 16#0B71#,
      Stencil_Test                  => 16#0B90#,
      Dither                        => 16#0BD0#,
      Blend                         => 16#0BE2#,
      Color_Logic_Op                => 16#0BF2#,
      Scissor_Test                  => 16#0C11#,
      Polygon_Offset_Point          => 16#2A01#,
      Polygon_Offset_Line           => 16#2A02#,
      Clip_Distance0                => 16#3000#,
      Clip_Distance1                => 16#3001#,
      Clip_Distance2                => 16#3002#,
      Clip_Distance3                => 16#3003#,
      Clip_Distance4                => 16#3004#,
      Clip_Distance5                => 16#3005#,
      Clip_Distance6                => 16#3006#,
      Clip_Distance7                => 16#3007#,
      Polygon_Offset_Fill           => 16#8037#,
      Multisample                   => 16#809D#,
      Sample_Alpha_To_Coverage      => 16#809E#,
      Sample_Alpha_To_One           => 16#809F#,
      Sample_Coverage               => 16#80A0#,
      Debug_Output_Synchronous      => 16#8242#,
      Program_Point_Size            => 16#8642#,
      Depth_Clamp                   => 16#864F#,
      Texture_Cube_Map_Seamless     => 16#884F#,
      Sample_Shading                => 16#8C36#,
      Rasterizer_Discard            => 16#8C89#,
      Primitive_Restart_Fixed_Index => 16#8D69#,
      Framebuffer_Srgb              => 16#8DB9#,
      Sample_Mask                   => 16#8E51#,
      Primitive_Restart             => 16#8F9D#,
      Debug_Output                  => 16#92E0#);

   --  typedef void (APIENTRYP PFNGLENABLEPROC) (GLenum cap);
   --  GLAPI void APIENTRY glEnable (GLenum cap);
   type Enable_Ptr is access procedure (Capability : Capabilities) with
     Convention => C;

   Enable : Enable_Ptr := null;

   --  typedef void (APIENTRYP PFNGLDISABLEPROC) (GLenum cap);
   --  GLAPI void APIENTRY glDisable (GLenum cap);
   type Disable_Ptr is access procedure (Capability : Capabilities) with
     Convention => C;

   Disable : Disable_Ptr := null;
end GL;
