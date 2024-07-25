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

   --  function Init_ is new Video.GL.Get_Subprogram
   --    (Subprogram_Name       => "",
   --     Access_To_Sub_Program => );

   procedure Initialise is
   begin
      Clear_Colour   := Init_Clear_Colour;
      Clear          := Init_Clear;
      Create_Shader  := Init_Create_Shader;
      Shader_Source  := Init_Shader_Source;
      Compile_Shader := Init_Compile_Shader;
      Create_Program := Init_Create_Program;
      Attach_Shader  := Init_Attach_Shader;
   end Initialise;
end GL.SDL;
