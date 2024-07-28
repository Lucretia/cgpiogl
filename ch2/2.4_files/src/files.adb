with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO;
with Interfaces.C;
--  with Ada.Real_Time;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Timers;
with SDL.Video.GL;
with SDL.Video.Windows.Makers;
with GL.SDL;

procedure Files is
   package L1 renames Ada.Characters.Latin_1;
   package US renames Ada.Strings.Unbounded;
   --  package RT renames Ada.Real_Time;
   package Encoders renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   package IO renames Ada.Text_IO;
   package C renames Interfaces.C;
   package Timers renames SDL.Timers;
   package Video renames SDL.Video;
   package Windows renames Video.Windows;
   package Events renames SDL.Events.Events;

   Prog_Window : Windows.Window;
   Window_Size : constant SDL.Positive_Sizes := (600, 600);
   Context     : Video.GL.Contexts;
   Event       : Events.Events;
   Finished    : Boolean := False;

   --  function Nanoseconds return RT.Time_Span is (RT.Nanoseconds (RT.Clock));

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

   function Create_Shader_Program return GL.UInt is
      Vertex_Shader_Source   : String := Get_Shader_Source ("src/vertex_shader.glsl");
      Fragment_Shader_Source : String := Get_Shader_Source ("src/fragment_shader.glsl");

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

   Rendering_Program : GL.UInt;
   Total_VAOs        : constant := 1;
   VAOs              : GL.UInt_Array (1 .. Total_VAOs);

   procedure Initialise (Window : Windows.Window) is
   begin
      GL.SDL.Initialise;  -- GL entry points.

      Rendering_Program := Create_Shader_Program;

      GL.Gen_Vertex_Arrays (VAOs'Length, VAOs);
      GL.Bind_Vertex_Array (VAOs (1));
   end Initialise;

   procedure Display (Window : Windows.Window; Current_Time_MS : Timers.Milliseconds_Long) is
   begin
      GL.Clear_Colour (Red => 0.0, Green => 0.0, Blue => 0.0, Alpha => 1.0);
      GL.Clear (GL.Color_Buffer_Bit);

      GL.Use_Program (Rendering_Program);
      GL.Point_Size (30.0);
      GL.Draw_Arrays (GL.Points, 0, 1);
   end Display;

   use type SDL.Events.Keyboards.Key_Codes;
begin
   if SDL.Initialise then
      Video.GL.Set_Context_Profile (Video.GL.Core);
      Video.GL.Set_Core_Context_Profile (Major => 4, Minor => 6);

      Windows.Makers.Create
        (Win      => Prog_Window,
         Title    => Encoders.Encode ("Chapter 2: Program 3"),
         Position => Windows.Centered_Window_Position,
         Size     => Window_Size);

      Video.GL.Create (Context, From => Prog_Window);
      Video.GL.Set_Current (Context, Prog_Window);
      Video.GL.Set_Swap_Interval (Video.GL.Synchronised);

      Initialise (Prog_Window);

      Main : loop
         --  This is basically the "glfwWindowShouldClose" call.
         while Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Finished := True;

               when SDL.Events.Keyboards.Key_Up =>
                  if Event.Keyboard.Key_Sym.Key_Code = SDL.Events.Keyboards.Code_Escape then
                     Finished := True;
                  end if;

               when others =>
                  null;
            end case;
         end loop;

         Display (Prog_Window, Timers.Ticks);
         Video.GL.Swap (Prog_Window);

         exit Main when Finished;
      end loop Main;

      Windows.Finalize (Prog_Window);
      SDL.Quit;
   end if;
end Files;
