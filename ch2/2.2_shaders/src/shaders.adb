with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO;
--  with Ada.Real_Time;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Timers;
with SDL.Video.GL;
with SDL.Video.Windows.Makers;
with GL.SDL;

procedure Shaders is
   package L1 renames Ada.Characters.Latin_1;
   package US renames Ada.Strings.Unbounded;
   --  package RT renames Ada.Real_Time;
   package Encoders renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   package IO renames Ada.Text_IO;
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

   function Create_Shader_Program return GL.UInt is
      Vertex_Shader_Source : GL.Shader_Program_Array (1 .. 4) := (
         US.To_Unbounded_String ("#version 460" & L1.LF),
         US.To_Unbounded_String ("void main(void) {" & L1.LF),
         US.To_Unbounded_String ("  gl_Position = vec4(0.0, 0.0, 0.0, 1.0);" & L1.LF),
         US.To_Unbounded_String ("}" & L1.LF));
      Fragment_Shader_Source : GL.Shader_Program_Array (1 .. 5) := (
         US.To_Unbounded_String ("#version 460" & L1.LF),
         US.To_Unbounded_String ("out vec4 colour;" & L1.LF),
         US.To_Unbounded_String ("void main(void) {" & L1.LF),
         US.To_Unbounded_String ("  colour = vec4(0.0, 0.0, 1.0, 1.0);" & L1.LF),
         US.To_Unbounded_String ("}" & L1.LF));

      Vertex_Shader           : GL.UInt := GL.Create_Shader (Shader_Type => GL.Vertex);
      Fragment_Shader         : GL.UInt := GL.Create_Shader (Shader_Type => GL.Fragment);
      Vertex_Shader_Program   : GL.Shader_Programs := GL.Convert (Vertex_Shader_Source);
      Fragment_Shader_Program : GL.Shader_Programs := GL.Convert (Fragment_Shader_Source);
      Program_Result          : GL.UInt;
   begin
      GL.Shader_Source (Vertex_Shader, 1, Vertex_Shader_Program.Program, Vertex_Shader_Program.String_Lengths);
      GL.Shader_Source (Fragment_Shader, 1, Fragment_Shader_Program.Program, Fragment_Shader_Program.String_Lengths);
      GL.Compile_Shader (Vertex_Shader);
      GL.Compile_Shader (Fragment_Shader);

      Program_Result := GL.Create_Program.all;

      GL.Attach_Shader (Program_Result, Vertex_Shader);
      GL.Attach_Shader (Program_Result, Fragment_Shader);
      GL.Link_Program (Program_Result);

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
      GL.Clear_Colour (Red => 1.0, Green => 0.0, Blue => 0.0, Alpha => 1.0);
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
         Title    => Encoders.Encode ("Chapter 2: Program 2"),
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
end Shaders;