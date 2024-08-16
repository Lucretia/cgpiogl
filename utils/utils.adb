with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Interfaces.C;

package body Utils is
   package C renames Interfaces.C;
   package IO renames Ada.Text_IO;

   procedure Put_Shader_Log (Shader : GL.UInt) is
      Length : GL.Int;

      use type GL.Int;
   begin
      GL.Get_Shader (Shader, GL.Info_Log_Length, Length);

      if Length > 0 then
         declare
            Buffer  : C.char_array (0 .. C.size_t (Length));
            Written : GL.SizeI;
         begin
            GL.Get_Shader_Info_Log (Shader, Buffer'Length, Written, Buffer);

            IO.Put_Line ("Shader Info Log: " & C.To_Ada (Buffer));
         end;
      end if;
   end Put_Shader_Log;


   procedure Put_Program_Log (Program : GL.UInt) is
      Length : GL.Int;

      use type GL.Int;
   begin
      GL.Get_Program (Program, GL.Info_Log_Length, Length);

      if Length > 0 then
         declare
            Buffer  : C.char_array (0 .. C.size_t (Length));
            Written : GL.SizeI;
         begin
            GL.Get_Program_Info_Log (Program, Buffer'Length, Written, Buffer);

            IO.Put_Line ("Program Info Log: " & C.To_Ada (Buffer));
         end;
      end if;
   end Put_Program_Log;


   function Check_OpenGL_Error return Boolean is
      Found_Error : Boolean   := False;
      GL_Error    : GL.Errors := GL.Get_Error.all;

      use type GL.Errors;
   begin
      while GL_Error /= GL.No_Error loop
         IO.Put_Line ("GL Error: " & GL_Error'Image);

         Found_Error := True;

         GL_Error := GL.Get_Error.all;
      end loop;

      return Found_Error;
   end Check_OpenGL_Error;


   --  No point returning an Ada array when we're just giving it to a C function.
   function Get_Shader_Source (Filename : String) return String is
      package Dirs renames Ada.Directories;

      Length : Natural := Positive (Dirs.Size (Filename));
      Buffer : String (1 .. Length);

      package Char_IO is new Ada.Sequential_IO (Character);

      File  : Char_IO.File_Type;
      Index : Positive := Positive'First;
   begin
      Char_IO.Open (File, Char_IO.In_File, Filename);

      while not Char_IO.End_Of_File (File) loop
         Char_IO.Read (File, Buffer (Index));

         Index := @ + 1;
      end loop;

      return Buffer;
   end Get_Shader_Source;


   function Create_Shader_Program (VP_Filename, FP_Filename : String) return GL.UInt is
      Vertex_Shader_Source   : String := Get_Shader_Source (VP_Filename);
      Fragment_Shader_Source : String := Get_Shader_Source (FP_Filename);

      Vertex_Shader           : GL.UInt := GL.Create_Shader (Shader_Type => GL.Vertex);
      Fragment_Shader         : GL.UInt := GL.Create_Shader (Shader_Type => GL.Fragment);
      Vertex_Shader_Program   : GL.Shader_Programs := GL.Convert (Vertex_Shader_Source);
      Fragment_Shader_Program : GL.Shader_Programs := GL.Convert (Fragment_Shader_Source);
      Program_Result          : GL.UInt;
   begin
      GL.Shader_Source
        (Vertex_Shader,
         Vertex_Shader_Program.Program'Length,
         Vertex_Shader_Program.Program,
         Vertex_Shader_Program.String_Lengths);
      GL.Shader_Source
        (Fragment_Shader,
         Fragment_Shader_Program.Program'Length,
         Fragment_Shader_Program.Program,
         Fragment_Shader_Program.String_Lengths);
      GL.Compile_Shader (Vertex_Shader);

      if Check_OpenGL_Error then
         declare
            Compiled : GL.Int;
         begin
            GL.Get_Shader (Vertex_Shader, GL.Compile_Status, Compiled);

            if not Boolean'Val (Compiled) then
               IO.Put_Line ("Vertex compilation failed.");

               Put_Shader_Log (Vertex_Shader);
            end if;
         end;
      end if;

      GL.Compile_Shader (Fragment_Shader);

      if Check_OpenGL_Error then
         declare
            Compiled : GL.Int;
         begin
            GL.Get_Shader (Fragment_Shader, GL.Compile_Status, Compiled);

            if not Boolean'Val (Compiled) then
               IO.Put_Line ("Fragment compilation failed.");

               Put_Shader_Log (Fragment_Shader);
            end if;
         end;
      end if;

      Program_Result := GL.Create_Program.all;

      GL.Attach_Shader (Program_Result, Vertex_Shader);
      GL.Attach_Shader (Program_Result, Fragment_Shader);
      GL.Link_Program (Program_Result);

      if Check_OpenGL_Error then
         declare
            Linked : GL.Int;
         begin
            GL.Get_Program (Fragment_Shader, GL.Link_Status, Linked);

            if not Boolean'Val (Linked) then
               IO.Put_Line ("Linking failed.");

               Put_Program_Log (Program_Result);
            end if;
         end;
      end if;

      return Program_Result;
   end Create_Shader_Program;
end Utils;
