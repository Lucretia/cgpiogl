with GL;

package Utils is
   procedure Put_Shader_Log (Shader : GL.UInt);
   procedure Put_Program_Log (Program : GL.UInt);
   function Check_OpenGL_Error return Boolean;

   function Get_Shader_Source (Filename : String) return String;
   function Create_Shader_Program (VP_Filename, FP_Filename : String) return GL.UInt;
--     function Create_Shader_Program (VP_Filename, GP_Filename, FP_Filename : String) return GL.UInt;
--     function Create_Shader_Program (VP_Filename, TCS_Filename, TES_Filename, FP_Filename : String) return GL.UInt;
--     function Create_Shader_Program (VP_Filename, TCS_Filename, TES_Filename, GP_Filename, FP_Filename : String) return GL.UInt;
end Utils;
