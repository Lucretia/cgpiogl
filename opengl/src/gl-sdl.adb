with SDL.Video.GL;

package body GL.SDL is
   --  Have to qualify with Standard as SDL is ambiguous here.
   package Video renames Standard.SDL.Video;

   function Init_Clear_Colour is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glClearColor",
      Access_To_Sub_Program => Clear_Colour_Ptr);

   function Init_Clear is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glClear",
      Access_To_Sub_Program => Clear_Ptr);

   function Init_Create_Shader is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glCreateShader",
      Access_To_Sub_Program => Create_Shader_Ptr);

   function Init_Shader_Source is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glShaderSource",
      Access_To_Sub_Program => Shader_Source_Ptr);

   function Init_Compile_Shader is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glCompileShader",
      Access_To_Sub_Program => Compile_Shader_Ptr);

   function Init_Create_Program is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glCreateProgram",
      Access_To_Sub_Program => Create_Program_Ptr);

   function Init_Attach_Shader is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glAttachShader",
      Access_To_Sub_Program => Attach_Shader_Ptr);

   function Init_Link_Program is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glLinkProgram",
      Access_To_Sub_Program => Link_Program_Ptr);

   function Init_Gen_Vertex_Arrays is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glGenVertexArrays",
      Access_To_Sub_Program => Gen_Vertex_Arrays_Ptr);

   function Init_Bind_Buffer is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glBindBuffer",
      Access_To_Sub_Program => Bind_Buffer_Ptr);

   function Init_Gen_Buffers is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glGenBuffers",
      Access_To_Sub_Program => Gen_Buffers_Ptr);

   function Init_Buffer_Data is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glBufferData",
      Access_To_Sub_Program => Buffer_Data_Ptr);

   function Init_Bind_Vertex_Array is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glBindVertexArray",
      Access_To_Sub_Program => Bind_Vertex_Array_Ptr);

   function Init_Use_Program is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glUseProgram",
      Access_To_Sub_Program => Use_Program_Ptr);

   function Init_Draw_Arrays is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glDrawArrays",
      Access_To_Sub_Program => Draw_Arrays_Ptr);

   function Init_Point_Size is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glPointSize",
      Access_To_Sub_Program => Point_Size_Ptr);

   function Init_Get_Error is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glGetError",
      Access_To_Sub_Program => Get_Error_Ptr);

   function Init_Enable is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glEnable",
      Access_To_Sub_Program => Enable_Ptr);

   function Init_Disable is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glDisable",
      Access_To_Sub_Program => Disable_Ptr);

   function Init_Polygon_Mode is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "Polygon_Mode",
      Access_To_Sub_Program => Polygon_Mode_Ptr);

   function Init_Get_Shader is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glGetShaderiv",
      Access_To_Sub_Program => Get_Shader_Ptr);

   function Init_Get_Program is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glGetProgramiv",
      Access_To_Sub_Program => Get_Program_Ptr);

   function Init_Get_Shader_Info_Log is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glGetShaderInfoLog",
      Access_To_Sub_Program => Get_Shader_Info_Log_Ptr);

   function Init_Get_Program_Info_Log is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glGetProgramInfoLog",
      Access_To_Sub_Program => Get_Program_Info_Log_Ptr);

   function Init_Get_Uniform_Location is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glGetUniformLocation",
      Access_To_Sub_Program => Get_Uniform_Location_Ptr);

   function Init_Uniform is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glUniform1f",
      Access_To_Sub_Program => Uniform_Ptr);

   function Init_Uniform_Matrix is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glUniformMatrix4fv",
      Access_To_Sub_Program => Uniform_Matrix_Ptr);

   function Init_Vertex_Attrib_Pointer is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glVertexAttribPointer",
      Access_To_Sub_Program => Vertex_Attrib_Pointer_Ptr);

   function Init_Enable_Vertex_Attrib_Array is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glEnableVertexAttribArray",
      Access_To_Sub_Program => Enable_Vertex_Attrib_Array_Ptr);

   function Init_Depth_Func is new Video.GL.Get_Subprogram
     (Subprogram_Name       => "glDepthFunc",
      Access_To_Sub_Program => Depth_Func_Ptr);

   --  function Init_ is new Video.GL.Get_Subprogram
   --    (Subprogram_Name       => "",
   --     Access_To_Sub_Program => );

   procedure Initialise is
   begin
      Clear_Colour               := Init_Clear_Colour;
      Clear                      := Init_Clear;
      Create_Shader              := Init_Create_Shader;
      Shader_Source              := Init_Shader_Source;
      Compile_Shader             := Init_Compile_Shader;
      Create_Program             := Init_Create_Program;
      Attach_Shader              := Init_Attach_Shader;
      Link_Program               := Init_Link_Program;
      Gen_Vertex_Arrays          := Init_Gen_Vertex_Arrays;
      Bind_Buffer                := Init_Bind_Buffer;
      Gen_Buffers                := Init_Gen_Buffers;
      Buffer_Data                := Init_Buffer_Data;
      Bind_Vertex_Array          := Init_Bind_Vertex_Array;
      Use_Program                := Init_Use_Program;
      Draw_Arrays                := Init_Draw_Arrays;
      Point_Size                 := Init_Point_Size;
      Get_Error                  := Init_Get_Error;
      Enable                     := Init_Enable;
      Disable                    := Init_Disable;
      Polygon_Mode               := Init_Polygon_Mode;
      Get_Shader                 := Init_Get_Shader;
      Get_Program                := Init_Get_Program;
      Get_Shader_Info_Log        := Init_Get_Shader_Info_Log;
      Get_Program_Info_Log       := Init_Get_Program_Info_Log;
      Get_Uniform_Location       := Init_Get_Uniform_Location;
      Uniform                    := Init_Uniform;
      Uniform_Matrix             := Init_Uniform_Matrix;
      Vertex_Attrib_Pointer      := Init_Vertex_Attrib_Pointer;
      Enable_Vertex_Attrib_Array := Init_Enable_Vertex_Attrib_Array;
      Depth_Func                 := Init_Depth_Func;
   end Initialise;
end GL.SDL;
