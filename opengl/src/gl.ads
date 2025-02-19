with Ada.Finalization;
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with System;

package GL is
   package US renames Ada.Strings.Unbounded;
   package C renames Interfaces.C;

   type Bool    is new Interfaces.Unsigned_8; --  GLboolean
   type Float32 is new C.C_float;   --  GLfloat
   type UInt    is new C.unsigned;  --  GLuint
   type SizeI   is new C.int;       --  GLsizei
   type Int     is new C.int;       --  GLint

   type Int_Array is array (C.size_t range <>) of Int with
     Convention => C;
   type UInt_Array is array (C.size_t range <>) of UInt with
     Convention => C;

   type Float32_Array is array (C.size_t range <>) of Float32 with
     Convention => C;

   GL_True  : constant Bool := 0;
   GL_False : constant Bool := 0;

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
   function Convert (Shader_Program : String) return Shader_Programs;

   --  typedef void (APIENTRYP PFNGLSHADERSOURCEPROC) (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
   --  GLAPI void APIENTRY glShaderSource (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
   type Shader_Source_Ptr is access procedure
     (Shader  : UInt;
      Count   : SizeI;
      Program : C.Strings.chars_ptr_array;
      Length  : Int_Array -- TODO: This can be set to null. On null, it will generate the lengths internally.
     ) with
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

   procedure Generate_Vertex_Arrays (Arrays : in out UInt_Array) with
     Inline;

   --  typedef void (APIENTRYP PFNGLBINDVERTEXARRAYPROC) (GLuint array);
   --  GLAPI void APIENTRY glBindVertexArray (GLuint array);
   type Bind_Vertex_Array_Ptr is access procedure (Arr : UInt) with
     Convention => C;

   Bind_Vertex_Array : Bind_Vertex_Array_Ptr := null;

   --    <enum value="0x80EE" name="GL_PARAMETER_BUFFER" group="BufferTargetARB"/>
   --    <enum value="0x8892" name="GL_ARRAY_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x8893" name="GL_ELEMENT_ARRAY_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x88EB" name="GL_PIXEL_PACK_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x88EC" name="GL_PIXEL_UNPACK_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x8A11" name="GL_UNIFORM_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x8C2A" name="GL_TEXTURE_BUFFER" group="TextureTarget,CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x8C8E" name="GL_TRANSFORM_FEEDBACK_BUFFER" group="ProgramInterface,BufferTargetARB,BufferStorageTarget,CopyBufferSubDataTarget"/>
   --    <enum value="0x8F36" name="GL_COPY_READ_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x8F37" name="GL_COPY_WRITE_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x8F3F" name="GL_DRAW_INDIRECT_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x90D2" name="GL_SHADER_STORAGE_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x90EE" name="GL_DISPATCH_INDIRECT_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x9192" name="GL_QUERY_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>
   --    <enum value="0x92C0" name="GL_ATOMIC_COUNTER_BUFFER" group="CopyBufferSubDataTarget,BufferTargetARB,BufferStorageTarget"/>

   --  BufferTargetARB.
   type Buffer_Targets is
     (Parameter_Buffer,
      Array_Buffer,
      Element_Array_Buffer,
      Pixel_Pack_Buffer,
      Pixel_Unpack_Buffer,
      Uniform_Buffer,
      Texture_Buffer,
      Transform_Feedback_Buffer,
      Copy_Read_Buffer,
      Copy_Write_Buffer,
      Draw_Indirect_Buffer,
      Shader_Storage_Buffer,
      Dispatch_Indirect_Buffer,
      Query_Buffer,
      Atomic_Counter_Buffer) with
     Convention => C;

   for Buffer_Targets use
     (Parameter_Buffer          => 16#80EE#,
      Array_Buffer              => 16#8892#,
      Element_Array_Buffer      => 16#8893#,
      Pixel_Pack_Buffer         => 16#88EB#,
      Pixel_Unpack_Buffer       => 16#88EC#,
      Uniform_Buffer            => 16#8A11#,
      Texture_Buffer            => 16#8C2A#,
      Transform_Feedback_Buffer => 16#8C8E#,
      Copy_Read_Buffer          => 16#8F36#,
      Copy_Write_Buffer         => 16#8F37#,
      Draw_Indirect_Buffer      => 16#8F3F#,
      Shader_Storage_Buffer     => 16#90D2#,
      Dispatch_Indirect_Buffer  => 16#90EE#,
      Query_Buffer              => 16#9192#,
      Atomic_Counter_Buffer     => 16#92C0#);

   --  typedef void (APIENTRYP PFNGLBINDBUFFERPROC) (GLenum target, GLuint buffer);
   --  GLAPI void APIENTRY glBindBuffer (GLenum target, GLuint buffer);
   type Bind_Buffer_Ptr is access procedure (Target : Buffer_Targets; Buffers : in UInt) with
     Convention => C;

   Bind_Buffer : Bind_Buffer_Ptr := null;

   --  typedef void (APIENTRYP PFNGLGENBUFFERSPROC) (GLsizei n, GLuint *buffers);
   --  GLAPI void APIENTRY glGenBuffers (GLsizei n, GLuint *buffers);
   type Gen_Buffers_Ptr is access procedure (N : GL.SizeI; Buffers : in out UInt_Array) with
     Convention => C;

   Gen_Buffers : Gen_Buffers_Ptr := null;

   procedure Generate_Buffers (Buffers : in out UInt_Array) with
     Inline;

   --    <enum value="0x88E0" name="GL_STREAM_DRAW" group="VertexBufferObjectUsage,BufferUsageARB"/>
   --    <enum value="0x88E1" name="GL_STREAM_READ" group="VertexBufferObjectUsage,BufferUsageARB"/>
   --    <enum value="0x88E2" name="GL_STREAM_COPY" group="VertexBufferObjectUsage,BufferUsageARB"/>
   --    <enum value="0x88E4" name="GL_STATIC_DRAW" group="VertexBufferObjectUsage,BufferUsageARB"/>
   --    <enum value="0x88E5" name="GL_STATIC_READ" group="VertexBufferObjectUsage,BufferUsageARB"/>
   --    <enum value="0x88E6" name="GL_STATIC_COPY" group="VertexBufferObjectUsage,BufferUsageARB"/>
   --    <enum value="0x88E8" name="GL_DYNAMIC_DRAW" group="VertexBufferObjectUsage,BufferUsageARB"/>
   --    <enum value="0x88E9" name="GL_DYNAMIC_READ" group="VertexBufferObjectUsage,BufferUsageARB"/>
   --    <enum value="0x88EA" name="GL_DYNAMIC_COPY" group="VertexBufferObjectUsage,BufferUsageARB"/>

   --  BufferUsageARB
   type Buffer_Usages is
     (Stream_Draw,
      Stream_Read,
      Stream_Copy,
      Static_Draw,
      Static_Read,
      Static_Copy,
      Dynamic_Draw,
      Dynamic_Read,
      Dynamic_Copy) with
     Convention => C;

   for Buffer_Usages use
     (Stream_Draw  => 16#88E0#,
      Stream_Read  => 16#88E1#,
      Stream_Copy  => 16#88E2#,
      Static_Draw  => 16#88E4#,
      Static_Read  => 16#88E5#,
      Static_Copy  => 16#88E6#,
      Dynamic_Draw => 16#88E8#,
      Dynamic_Read => 16#88E9#,
      Dynamic_Copy => 16#88EA#);

   --  typedef void (APIENTRYP PFNGLBUFFERDATAPROC) (GLenum target, GLsizeiptr size, const void *data, GLenum usage);
   --  GLAPI void APIENTRY glBufferData (GLenum target, GLsizeiptr size, const void *data, GLenum usage);
   type Buffer_Data_Ptr is access procedure
     (Target : Buffer_Targets;
      Size   : GL.SizeI;
      Data   : in Float32_Array;  -- TODO: I bet this can be many types.
      Usage  : in Buffer_Usages) with
     Convention => C;

   Buffer_Data : Buffer_Data_Ptr := null;

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

   type Draw_Arrays_Instanced_Ptr is access procedure (Mode : Primitive_Modes; First : Int; Count, Instances : SizeI) with
     Convention => C;

   Draw_Arrays_Instanced : Draw_Arrays_Instanced_Ptr := null;

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
      Cap_Cull_Face,
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
      Cap_Cull_Face                 => 16#0B44#,
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

   --  #define GL_FRONT                          0x0404
   --  #define GL_BACK                           0x0405
   --  #define GL_FRONT_AND_BACK                 0x0408

   type Triangle_Faces is (Front, Back, Front_And_Back) with
     Convention => C;

   for Triangle_Faces use (Front          => 16#0404#,
                           Back           => 16#0405#,
                           Front_And_Back => 16#0408#);

   type Cull_Face_Ptr is access procedure (Mode : Triangle_Faces) with
     Convention => C;

   Cull_Face : Cull_Face_Ptr := null;

   type Front_Face_Direction is (CW, CCW) with
     Convention => C;

   for Front_Face_Direction use (CW => 16#0900#, CCW => 16#0901#);

   type Front_Face_Ptr is access procedure (Mode : Front_Face_Direction) with
     Convention => C;

   Front_Face : Front_Face_Ptr := null;


   --  #define GL_POINT                          0x1B00
   --  #define GL_LINE                           0x1B01
   --  #define GL_FILL                           0x1B02

   type Polygon_Modes is (Point, Line, Fill) with
     Convention => C;

   for Polygon_Modes use (Point => 16#1B00#,
                          Line  => 16#1B01#,
                          Fill  => 16#1B02#);

   --  typedef void (APIENTRYP PFNGLPOLYGONMODEPROC) (GLenum face, GLenum mode);
   --  GLAPI void APIENTRY glPolygonMode (GLenum face, GLenum mode);
   type Polygon_Mode_Ptr is access procedure (Face : Triangle_Faces; Mode : Polygon_Modes) with
     Convention => C;

   Polygon_Mode : Polygon_Mode_Ptr := null;

   --  #define GL_SHADER_TYPE                    0x8B4F
   --  #define GL_DELETE_STATUS                  0x8B80
   --  #define GL_COMPILE_STATUS                 0x8B81
   --  #define GL_INFO_LOG_LENGTH                0x8B84
   --  #define GL_SHADER_SOURCE_LENGTH           0x8B88

   type Shader_Parameter_Names is
     (Shader_Type,
      Delete_Status,
      Compile_Status,
      Info_Log_Length,
      Shader_Source_Length) with
     Convention => C;

   for Shader_Parameter_Names use
     (Shader_Type          => 16#8B4F#,
      Delete_Status        => 16#8B80#,
      Compile_Status       => 16#8B81#,
      Info_Log_Length      => 16#8B84#,
      Shader_Source_Length => 16#8B88#);

   --  typedef void (APIENTRYP PFNGLGETSHADERIVPROC) (GLuint shader, GLenum pname, GLint *params);
   --  GLAPI void APIENTRY glGetShaderiv (GLuint shader, GLenum pname, GLint *params);
   type Get_Shader_Ptr is access procedure (Shader : UInt; Parameter : Shader_Parameter_Names; Params : out Int) with
     Convention => C;

   Get_Shader : Get_Shader_Ptr := null;

   --  #define GL_COMPUTE_WORK_GROUP_SIZE        0x8267
   --  #define GL_PROGRAM_BINARY_LENGTH          0x8741
   --  #define GL_GEOMETRY_VERTICES_OUT          0x8916
   --  #define GL_GEOMETRY_INPUT_TYPE            0x8917
   --  #define GL_GEOMETRY_OUTPUT_TYPE           0x8918
   --  #define GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH 0x8A35
   --  #define GL_ACTIVE_UNIFORM_BLOCKS          0x8A36
   --  #define GL_DELETE_STATUS                  0x8B80
   --  #define GL_LINK_STATUS                    0x8B82
   --  #define GL_VALIDATE_STATUS                0x8B83
   --  #define GL_INFO_LOG_LENGTH                0x8B84
   --  #define GL_ATTACHED_SHADERS               0x8B85
   --  #define GL_ACTIVE_UNIFORMS                0x8B86
   --  #define GL_ACTIVE_UNIFORM_MAX_LENGTH      0x8B87
   --  #define GL_ACTIVE_ATTRIBUTES              0x8B89
   --  #define GL_ACTIVE_ATTRIBUTE_MAX_LENGTH    0x8B8A
   --  #define GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH 0x8C76
   --  #define GL_TRANSFORM_FEEDBACK_BUFFER_MODE 0x8C7F
   --  #define GL_TRANSFORM_FEEDBACK_VARYINGS    0x8C83
   --  #define GL_ACTIVE_ATOMIC_COUNTER_BUFFERS  0x92D9
   type Program_Properties is
     (Compute_Work_Group_Size,
      Program_Binary_Length,
      Geometry_Vertices_Out,
      Geometry_Input_Type,
      Geometry_Output_Type,
      Active_Uniform_Block_Max_Name_Length,
      Active_Uniform_Blocks,
      Delete_Status,
      Link_Status,
      Validate_Status,
      Info_Log_Length,
      Attached_Shaders,
      Active_Uniforms,
      Active_Uniform_Max_Length,
      Active_Attributes,
      Active_Attribute_Max_Length,
      Transform_Feedback_Varying_Max_Length,
      Transform_Feedback_Buffer_Mode,
      Transform_Feedback_Varyings,
      Active_Atomic_Counter_Buffers) with
     Convention => C;

   for Program_Properties use
     (Compute_Work_Group_Size               => 16#8267#,
      Program_Binary_Length                 => 16#8741#,
      Geometry_Vertices_Out                 => 16#8916#,
      Geometry_Input_Type                   => 16#8917#,
      Geometry_Output_Type                  => 16#8918#,
      Active_Uniform_Block_Max_Name_Length  => 16#8A35#,
      Active_Uniform_Blocks                 => 16#8A36#,
      Delete_Status                         => 16#8B80#,
      Link_Status                           => 16#8B82#,
      Validate_Status                       => 16#8B83#,
      Info_Log_Length                       => 16#8B84#,
      Attached_Shaders                      => 16#8B85#,
      Active_Uniforms                       => 16#8B86#,
      Active_Uniform_Max_Length             => 16#8B87#,
      Active_Attributes                     => 16#8B89#,
      Active_Attribute_Max_Length           => 16#8B8A#,
      Transform_Feedback_Varying_Max_Length => 16#8C76#,
      Transform_Feedback_Buffer_Mode        => 16#8C7F#,
      Transform_Feedback_Varyings           => 16#8C83#,
      Active_Atomic_Counter_Buffers         => 16#92D9#);

   --  typedef void (APIENTRYP PFNGLGETPROGRAMIVPROC) (GLuint program, GLenum pname, GLint *params);
   --  GLAPI void APIENTRY glGetProgramiv (GLuint program, GLenum pname, GLint *params);
   type Get_Program_Ptr is access procedure (Program : UInt; Parameter : Program_Properties; Params : out Int) with
     Convention => C;

   Get_Program : Get_Program_Ptr := null;

   --  typedef void (APIENTRYP PFNGLGETSHADERINFOLOGPROC) (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
   --  GLAPI void APIENTRY glGetShaderInfoLog (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
   type Get_Shader_Info_Log_Ptr is access procedure
     (Shader : UInt; Buffer_Size : SizeI; Length : out SizeI; Info_Log : out C.char_array) with
       Convention => C;

   Get_Shader_Info_Log : Get_Shader_Info_Log_Ptr := null;

   --  typedef void (APIENTRYP PFNGLGETPROGRAMINFOLOGPROC) (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
   --  GLAPI void APIENTRY glGetProgramInfoLog (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
   type Get_Program_Info_Log_Ptr is access procedure
     (Program : UInt; Buffer_Size : SizeI; Length : out SizeI; Info_Log : out C.char_array) with
       Convention => C;

   Get_Program_Info_Log : Get_Program_Info_Log_Ptr := null;

  --  typedef GLint (APIENTRYP PFNGLGETUNIFORMLOCATIONPROC) (GLuint program, const GLchar *name);
  --  GLAPI GLint APIENTRY glGetUniformLocation (GLuint program, const GLchar *name);
  type Get_Uniform_Location_Ptr is access function (Program : UInt; Name : C.char_array) return Int with
    Convention => C;

  Get_Uniform_Location : Get_Uniform_Location_Ptr := null;

  --  typedef void (APIENTRYP PFNGLUNIFORM1FPROC) (GLint location, GLfloat v0);
  --  GLAPI void APIENTRY glUniform1f (GLint location, GLfloat v0);
  type Uniform_Ptr is access procedure (Location : Int; Value : Float32) with
    Convention => C;

  Uniform : Uniform_Ptr := null;

   --  typedef void (APIENTRYP PFNGLUNIFORMMATRIX4FVPROC) (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
   --  GLAPI void APIENTRY glUniformMatrix4fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
   type Uniform_Matrix_Ptr is access procedure (Location : Int; Count : SizeI; Transpose : Bool ; Values : Float32_Array) with
     Convention => C;

   Uniform_Matrix : Uniform_Matrix_Ptr;

   --  <enum value="0x1400" name="GL_BYTE" group="VertexAttribIType,WeightPointerTypeARB,TangentPointerTypeEXT,BinormalPointerTypeEXT,ColorPointerType,ListNameType,NormalPointerType,PixelType,VertexAttribType,VertexAttribPointerType"/>
   --  <enum value="0x1401" name="GL_UNSIGNED_BYTE" group="VertexAttribIType,ScalarType,ReplacementCodeTypeSUN,ElementPointerTypeATI,MatrixIndexPointerTypeARB,WeightPointerTypeARB,ColorPointerType,DrawElementsType,ListNameType,PixelType,VertexAttribType,VertexAttribPointerType"/>
   --  <enum value="0x1402" name="GL_SHORT" group="VertexAttribIType,SecondaryColorPointerTypeIBM,WeightPointerTypeARB,TangentPointerTypeEXT,BinormalPointerTypeEXT,ColorPointerType,IndexPointerType,ListNameType,NormalPointerType,PixelType,TexCoordPointerType,VertexPointerType,VertexAttribType,VertexAttribPointerType"/>
   --  <enum value="0x1403" name="GL_UNSIGNED_SHORT" group="VertexAttribIType,ScalarType,ReplacementCodeTypeSUN,ElementPointerTypeATI,MatrixIndexPointerTypeARB,WeightPointerTypeARB,ColorPointerType,DrawElementsType,ListNameType,PixelFormat,PixelType,VertexAttribType,VertexAttribPointerType"/>
   --  <enum value="0x1404" name="GL_INT" group="VertexAttribIType,SecondaryColorPointerTypeIBM,WeightPointerTypeARB,TangentPointerTypeEXT,BinormalPointerTypeEXT,ColorPointerType,IndexPointerType,ListNameType,NormalPointerType,PixelType,TexCoordPointerType,VertexPointerType,VertexAttribType,AttributeType,UniformType,VertexAttribPointerType"/>
   --  <enum value="0x1405" name="GL_UNSIGNED_INT" group="VertexAttribIType,ScalarType,ReplacementCodeTypeSUN,ElementPointerTypeATI,MatrixIndexPointerTypeARB,WeightPointerTypeARB,ColorPointerType,DrawElementsType,ListNameType,PixelFormat,PixelType,VertexAttribType,AttributeType,UniformType,VertexAttribPointerType"/>
   --  <enum value="0x1406" name="GL_FLOAT" group="MapTypeNV,SecondaryColorPointerTypeIBM,WeightPointerTypeARB,VertexWeightPointerTypeEXT,TangentPointerTypeEXT,BinormalPointerTypeEXT,ColorPointerType,FogCoordinatePointerType,FogPointerTypeEXT,FogPointerTypeIBM,IndexPointerType,ListNameType,NormalPointerType,PixelType,TexCoordPointerType,VertexPointerType,VertexAttribType,AttributeType,UniformType,VertexAttribPointerType"/>
   --  <enum value="0x140A" name="GL_DOUBLE" group="VertexAttribLType,MapTypeNV,SecondaryColorPointerTypeIBM,WeightPointerTypeARB,TangentPointerTypeEXT,BinormalPointerTypeEXT,ColorPointerType,FogCoordinatePointerType,FogPointerTypeEXT,FogPointerTypeIBM,IndexPointerType,NormalPointerType,TexCoordPointerType,VertexPointerType,VertexAttribType,AttributeType,UniformType,VertexAttribPointerType"/>
   --  <enum value="0x140B" name="GL_HALF_FLOAT" group="PixelType,VertexAttribPointerType,VertexAttribType"/>
   --  <enum value="0x140C" name="GL_FIXED" group="VertexAttribPointerType,VertexAttribType"/>
   --  <enum value="0x140E" name="GL_INT64_ARB" group="VertexAttribPointerType,AttributeType"/>
   --  <enum value="0x140E" name="GL_INT64_NV" group="VertexAttribPointerType,AttributeType"/>
   --  <enum value="0x140F" name="GL_UNSIGNED_INT64_ARB" group="VertexAttribPointerType,AttributeType"/>
   --  <enum value="0x140F" name="GL_UNSIGNED_INT64_NV" group="VertexAttribPointerType,AttributeType"/>
   --  <enum value="0x8368" name="GL_UNSIGNED_INT_2_10_10_10_REV" group="PixelType,VertexAttribPointerType,VertexAttribType"/>
   --  <enum value="0x8368" name="GL_UNSIGNED_INT_2_10_10_10_REV_EXT" group="PixelType,VertexAttribPointerType,VertexAttribType"/>
   --  <enum value="0x8C3B" name="GL_UNSIGNED_INT_10F_11F_11F_REV" group="PixelType,VertexAttribPointerType,VertexAttribType"/>
   --  <enum value="0x8D9F" name="GL_INT_2_10_10_10_REV" group="VertexAttribPointerType,VertexAttribType"/>
   type Vertex_Attrib_Pointer_Types is
     (GL_Byte,
      GL_Unsigned_Byte,
      GL_Short,
      GL_Unsigned_Short,
      GL_Int,
      GL_Unsigned_Int,
      GL_Float,
      GL_Double,
      GL_Half_Float,
      GL_Fixed,
      GL_Int64_Arb,
      --  GL_Int64_Nv,
      GL_Unsigned_Int64_Arb,
      --  GL_Unsigned_Int64_Nv,
      GL_Unsigned_Int_2_10_10_10_Rev,
      --  GL_Unsigned_Int_2_10_10_10_Rev_Ext,
      GL_Unsigned_Int_10_F_11_F_11_F_Rev,
      GL_Int_2_10_10_10_Rev) with
     Convention => C;

   for Vertex_Attrib_Pointer_Types use
     (GL_Byte                            => 16#1400#,
      GL_Unsigned_Byte                   => 16#1401#,
      GL_Short                           => 16#1402#,
      GL_Unsigned_Short                  => 16#1403#,
      GL_Int                             => 16#1404#,
      GL_Unsigned_Int                    => 16#1405#,
      GL_Float                           => 16#1406#,

      GL_Double                          => 16#140A#,
      GL_Half_Float                      => 16#140B#,
      GL_Fixed                           => 16#140C#,
      GL_Int64_Arb                       => 16#140E#,
      --  GL_Int64_Nv                        => 16#140E#,
      GL_Unsigned_Int64_Arb              => 16#140F#,
      --  GL_Unsigned_Int64_Nv               => 16#140F#,
      GL_Unsigned_Int_2_10_10_10_Rev     => 16#8368#,
      --  GL_Unsigned_Int_2_10_10_10_Rev_Ext => 16#8368#,
      GL_Unsigned_Int_10_F_11_F_11_F_Rev => 16#8C3B#,
      GL_Int_2_10_10_10_Rev              => 16#8D9F#);

   GL_Int64_Nv renames GL_Int64_Arb;
   GL_Unsigned_Int64_Nv renames GL_Unsigned_Int64_Arb;
   GL_Unsigned_Int_2_10_10_10_Rev_Ext renames GL_Unsigned_Int_2_10_10_10_Rev;

   --  typedef void (APIENTRYP PFNGLVERTEXATTRIBPOINTERPROC) (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer);
   --  GLAPI void APIENTRY glVertexAttribPointer (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer);
   type Vertex_Attrib_Pointer_Ptr is access procedure
     (Index      : UInt;
      Size       : Int;
      PType      : Vertex_Attrib_Pointer_Types;
      Normalized : Bool;
      Stride     : SizeI;
      Pointer    : System.Address);  -- TODO: WTF?

   Vertex_Attrib_Pointer : Vertex_Attrib_Pointer_Ptr := null;

   --  typedef void (APIENTRYP PFNGLENABLEVERTEXATTRIBARRAYPROC) (GLuint index);
   --  GLAPI void APIENTRY glEnableVertexAttribArray (GLuint index);
   type Enable_Vertex_Attrib_Array_Ptr is access procedure (Index : UInt) with
     Convention => C;

   Enable_Vertex_Attrib_Array : Enable_Vertex_Attrib_Array_Ptr := null;

   --  <enum value="0x0200" name="GL_NEVER" group="StencilFunction,IndexFunctionEXT,AlphaFunction,DepthFunction"/>
   --  <enum value="0x0201" name="GL_LESS" group="StencilFunction,IndexFunctionEXT,AlphaFunction,DepthFunction"/>
   --  <enum value="0x0202" name="GL_EQUAL" group="StencilFunction,IndexFunctionEXT,AlphaFunction,DepthFunction"/>
   --  <enum value="0x0203" name="GL_LEQUAL" group="StencilFunction,IndexFunctionEXT,AlphaFunction,DepthFunction"/>
   --  <enum value="0x0204" name="GL_GREATER" group="StencilFunction,IndexFunctionEXT,AlphaFunction,DepthFunction"/>
   --  <enum value="0x0205" name="GL_NOTEQUAL" group="StencilFunction,IndexFunctionEXT,AlphaFunction,DepthFunction"/>
   --  <enum value="0x0206" name="GL_GEQUAL" group="StencilFunction,IndexFunctionEXT,AlphaFunction,DepthFunction"/>
   --  <enum value="0x0207" name="GL_ALWAYS" group="StencilFunction,IndexFunctionEXT,AlphaFunction,DepthFunction"/>
   type Depth_Functions is
     (Never,
      Less,
      Equal,
      L_Equal,
      Greater,
      Not_Equal,
      G_Equal,
      Always) with
     Convention => C;

   for Depth_Functions use
     (Never     => 16#0200#,
      Less      => 16#0201#,
      Equal     => 16#0202#,
      L_Equal   => 16#0203#,
      Greater   => 16#0204#,
      Not_Equal => 16#0205#,
      G_Equal   => 16#0206#,
      Always    => 16#0207#);

   --  typedef void (APIENTRYP PFNGLDEPTHFUNCPROC) (GLenum func);
   --  GLAPI void APIENTRY glDepthFunc (GLenum func);
   type Depth_Func_Ptr is access procedure (Func : Depth_Functions) with
     Convention => C;

   Depth_Func : Depth_Func_Ptr := null;

   type Viewport_Ptr is access procedure (X, Y : Int; Width, Height : SizeI) with
     Convention => C;

   Viewport : Viewport_Ptr := null;
end GL;
