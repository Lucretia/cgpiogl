--  with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
--  with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C;
--  with Ada.Real_Time;
with Maths.Matrix4s;
with Maths.Matrix4s.Stacks;
with Maths.Utils;
with Maths.Vector4s;
with SDL;
with SDL.Events;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Windows;
with SDL.Timers;
with SDL.Video;
with SDL.Video.GL;
with SDL.Video.Surfaces;
with SDL.Video.Renderers;
with SDL.Video.Windows.Makers;
with System;
with GL.SDL;
with Utils;

procedure Simple_Solar_System is
   --  package RT renames Ada.Real_Time;
   package Encoders renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   --  package IO renames Ada.Text_IO;
   package C renames Interfaces.C;
   package Timers renames SDL.Timers;
   package Video renames SDL.Video;
   package Surfaces renames Video.Surfaces;
   package Renderers renames SDL.Video.Renderers;
   package Windows renames Video.Windows;
   package Events renames SDL.Events.Events;
   package Matrix4s renames Maths.Matrix4s;
   package Vector4s renames Maths.Vector4s;

   subtype GL_Matrix is GL.Float32_Array (1 .. 16);
   function Convert is new Ada.Unchecked_Conversion (Source => Matrix4s.Float_Array, Target => GL_Matrix);

   Prog_Window : Windows.Window;
   Window_Size : constant SDL.Positive_Sizes := (600, 600);
   Context     : Video.GL.Contexts;
   Event       : Events.Events;
   Finished    : Boolean := False;

   --  function Nanoseconds return RT.Time_Span is (RT.Nanoseconds (RT.Clock));

   Camera_X          : constant Float :=  0.0;
   Camera_Y          : constant Float :=  0.0;
   Camera_Z          : constant Float :=  8.0;
   Cube_Pos_X        : constant Float :=  0.0;  --  cubeLocX/Y/Z in the book.
   Cube_Pos_Y        : constant Float := -2.0;  --  Shift Y down to reveal perspective.
   Cube_Pos_Z        : constant Float :=  0.0;
   Pyramid_Pos_X     : constant Float :=  2.0;
   Pyramid_Pos_Y     : constant Float :=  2.0;
   Pyramid_Pos_Z     : constant Float :=  0.0;

   use type C.size_t;

   Rendering_Program : GL.UInt;
   Total_VAOs        : constant := 1;
   Total_VBOs        : constant := 2;
   VAOs              : GL.UInt_Array (1 .. Total_VAOs);
   VBOs              : GL.UInt_Array (1 .. Total_VBOs);
   Cube_VBO          : constant C.size_t := (VBOs'First);
   Pyramid_VBO       : constant C.size_t := Cube_VBO + 1;

   --  package Trig is new Ada.Numerics.Generic_Elementary_Functions (Float);

   use type GL.Float32;

   --  36 vertices, 12 triangles, makes 2x2x2 cube placed at origin.
   Cube_Vertices : constant GL.Float32_Array (1 .. 108) :=
      (-1.0,  1.0, -1.0, -1.0, -1.0, -1.0,  1.0, -1.0, -1.0,
        1.0, -1.0, -1.0,  1.0,  1.0, -1.0, -1.0,  1.0, -1.0,
        1.0, -1.0, -1.0,  1.0, -1.0,  1.0,  1.0,  1.0, -1.0,
        1.0, -1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0, -1.0,
        1.0, -1.0,  1.0, -1.0, -1.0,  1.0,  1.0,  1.0,  1.0,
       -1.0, -1.0,  1.0, -1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
       -1.0, -1.0,  1.0, -1.0, -1.0, -1.0, -1.0,  1.0,  1.0,
       -1.0, -1.0, -1.0, -1.0,  1.0, -1.0, -1.0,  1.0,  1.0,
       -1.0, -1.0,  1.0,  1.0, -1.0,  1.0,  1.0, -1.0, -1.0,
        1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,  1.0,
       -1.0,  1.0, -1.0,  1.0,  1.0, -1.0,  1.0,  1.0,  1.0,
        1.0,  1.0,  1.0, -1.0,  1.0,  1.0, -1.0,  1.0, -1.0);

   --  Pyramid with 18 vertices, comprising 6 triangles (four sides, and two on the bottom).
   Pyramid_Vertices : constant GL.Float32_Array (1 .. 54) :=
      (-1.0, -1.0,  1.0,  1.0, -1.0,  1.0,  0.0,  1.0,  0.0,  --  front face
        1.0, -1.0,  1.0,  1.0, -1.0, -1.0,  0.0,  1.0,  0.0,  --  right face
        1.0, -1.0, -1.0, -1.0, -1.0, -1.0,  0.0,  1.0,  0.0,  --  back face
       -1.0, -1.0, -1.0, -1.0, -1.0,  1.0,  0.0,  1.0,  0.0,  --  left face
       -1.0, -1.0, -1.0,  1.0, -1.0,  1.0, -1.0, -1.0,  1.0,  --  base – left front
        1.0, -1.0,  1.0, -1.0, -1.0, -1.0,  1.0, -1.0, -1.0   --  base – right back
      );

   Model_View_Stack : aliased Maths.Matrix4s.Stacks.Stack;

   procedure Initialise (Window : Windows.Window) is
      procedure Set_Up_Vertices is
         use type GL.SizeI;
      begin
         GL.Generate_Vertex_Arrays (VAOs);
         GL.Bind_Vertex_Array (VAOs (1));
         GL.Generate_Buffers (VBOs);

         GL.Bind_Buffer (GL.Array_Buffer, VBOs (Cube_VBO));
         GL.Buffer_Data (GL.Array_Buffer, Cube_Vertices'Length * System.Storage_Unit, Cube_Vertices, GL.Static_Draw);

         GL.Bind_Buffer (GL.Array_Buffer, VBOs (Pyramid_VBO));
         GL.Buffer_Data
           (GL.Array_Buffer,
            Pyramid_Vertices'Length * System.Storage_Unit,
            Pyramid_Vertices,
            GL.Static_Draw);

         Model_View_Stack.Init;

      end Set_Up_Vertices;
   begin
      GL.SDL.Initialise;  -- GL entry points.

      Rendering_Program := Utils.Create_Shader_Program ("src/vertex_shader.glsl", "src/fragment_shader.glsl");

      Set_Up_Vertices;
   end Initialise;

   Perspective,
   View,
   Model,
   Model_View     : Maths.Matrix4s.Matrix4 (Maths.Matrix4s.Components);

   procedure Display (Window : Windows.Window; Current_Time_MS : Timers.Milliseconds_Long) is
      MV_Location,
      P_Location     : GL.Int;
      --  Width, Height  : SDL.Natural_Dimension;
      Speed          : constant Float := Float (Current_Time_MS) / 1000.0;

      package Trig renames Maths.Utils.Trig;

      use type GL.Clear_Buffer_Mask;
      use type GL.SizeI;
      --  use type SDL.Dimension;
      use type Matrix4s.Matrix4;
   begin
      GL.Clear (GL.Depth_Buffer_Bit or GL.Color_Buffer_Bit);
      GL.Use_Program (Rendering_Program);

      --  Get the uniform variables for the MV and projection matrices.
      MV_Location := GL.Get_Uniform_Location (Rendering_Program, C.To_C ("mv_matrix"));
      P_Location  := GL.Get_Uniform_Location (Rendering_Program, C.To_C ("p_matrix"));

      --  Build perspective matrix.
      --  Video.GL.Get_Drawable_Size (Prog_Window, Width, Height);

      GL.Uniform_Matrix (P_Location, 1, GL.GL_False, Convert (Perspective.Elements));

      --  Build view matrix, model matrix, and model-view matrix.
      View := Matrix4s.Translate (-Camera_X, -Camera_Y, -Camera_Z);

      Model_View_Stack.Push (View);

      ------------------------------------------------------------------------------------------------------------------
      --  Pyramid = sun.
      ------------------------------------------------------------------------------------------------------------------
      Model_View_Stack.Push_Top;
      --  Model_View_Stack.Top := @ * Matrix4s.Translate (0.0, 0.0, 0.0);
      Model_View_Stack.Top := Model_View_Stack.Top * Matrix4s.Translate (0.0, 0.0, 0.0);
      Model_View_Stack.Push_Top;
      --  Model_View_Stack.Top := @ * Matrix4s.Rotate_Around_X (Float (Current_Time_MS));
      Model_View_Stack.Top := Model_View_Stack.Top * Matrix4s.Rotate_Around_X (Float (Speed));

      --  Copy perspective and MV matrices to corresponding uniform variables.
      GL.Uniform_Matrix (MV_Location, 1, GL.GL_False, Convert (Model_View_Stack.Top.Elements));

      --  Associate VBO with the corresponding vertex attribute in the vertex shader.
      GL.Bind_Buffer (GL.Array_Buffer, VBOs (Pyramid_VBO));
      GL.Vertex_Attrib_Pointer (0, 3, GL.GL_Float, GL.GL_False, 0, System.Null_Address);
      GL.Enable_Vertex_Attrib_Array (0);

      --  Adjust OpenGL settings and draw model.
      GL.Enable (GL.Depth_Test);
      GL.Depth_Func (GL.L_Equal);
      GL.Draw_Arrays (GL.Triangles, 0, Pyramid_Vertices'Length / 3);

      Model_View_Stack.Pop;

      ------------------------------------------------------------------------------------------------------------------
      --  Cube = planet.
      ------------------------------------------------------------------------------------------------------------------
      Model_View_Stack.Push_Top;
      Model_View_Stack.Top := Model_View_Stack.Top * Matrix4s.Translate
        (X => Trig.Sin (Speed) * 4.0,
         Y => 0.0,
         Z => Trig.Cos (Speed) * 4.0);
      Model_View_Stack.Push_Top;
      Model_View_Stack.Top := Model_View_Stack.Top * Matrix4s.Rotate_Around_Y (Speed);

      --  Copy perspective and MV matrices to corresponding uniform variables.
      GL.Uniform_Matrix (MV_Location, 1, GL.GL_False, Convert (Model_View_Stack.Top.Elements));

      --  Associate VBO with the corresponding vertex attribute in the vertex shader.
      GL.Bind_Buffer (GL.Array_Buffer, VBOs (Cube_VBO));
      GL.Vertex_Attrib_Pointer (0, 3, GL.GL_Float, GL.GL_False, 0, System.Null_Address);
      GL.Enable_Vertex_Attrib_Array (0);

      --  Adjust OpenGL settings and draw model.
      --  GL.Enable (GL.Depth_Test);
      --  GL.Depth_Func (GL.L_Equal);
      GL.Draw_Arrays (GL.Triangles, 0, Cube_Vertices'Length / 3);

      Model_View_Stack.Pop;

      ------------------------------------------------------------------------------------------------------------------
      --  Smaller cube = moon.
      ------------------------------------------------------------------------------------------------------------------
      Model_View_Stack.Push_Top;
      Model_View_Stack.Top := Model_View_Stack.Top * Matrix4s.Translate
        (X => 0.0,
         Y => Trig.Sin (Speed) * 2.0,
         Z => Trig.Cos (Speed) * 2.0);
      --  Model_View_Stack.Push_Top;
      Model_View_Stack.Top := Model_View_Stack.Top * Matrix4s.Rotate_Around_Z (Speed);
      Model_View_Stack.Top := Model_View_Stack.Top * Matrix4s.Scale (0.25);

      --  Copy perspective and MV matrices to corresponding uniform variables.
      GL.Uniform_Matrix (MV_Location, 1, GL.GL_False, Convert (Model_View_Stack.Top.Elements));

      --  Associate VBO with the corresponding vertex attribute in the vertex shader.
      GL.Bind_Buffer (GL.Array_Buffer, VBOs (Cube_VBO));
      GL.Vertex_Attrib_Pointer (0, 3, GL.GL_Float, GL.GL_False, 0, System.Null_Address);
      GL.Enable_Vertex_Attrib_Array (0);

      --  Adjust OpenGL settings and draw model.
      GL.Draw_Arrays (GL.Triangles, 0, Cube_Vertices'Length / 3);

      --  Remove moon scale/rotation/position, planet position, sun position, and view matrices from stack.
      Model_View_Stack.Pop;
      Model_View_Stack.Pop;
      Model_View_Stack.Pop;
      Model_View_Stack.Pop;

      --  IO.Put_Line ("Vertices'Length: " & Vertices'Length'Image); --  & "    Vertices'Length / 3: " & (Vertices'Length / 3)'Image');
   end Display;


   procedure Window_Resize (Window_Id : Video.Windows.ID; New_Size : SDL.Positive_Sizes) is
      Aspect : constant Float := Float (New_Size.Width) / Float (New_Size.Height);
      --  Aspect : Float;
      --  Width, Height  : SDL.Natural_Dimension;
   begin
      --  Video.GL.Get_Drawable_Size (Prog_Window, Width, Height);
      --  Aspect := Float (Width) / Float (Height);

      GL.Viewport (0, 0, GL.SizeI (New_Size.Width), GL.SizeI (New_Size.Height));

      Perspective := Matrix4s.Perspective
        (Field_of_View => Maths.Utils.To_Radians (Angle_Degrees => 60.0),
         Aspect_Ratio  => Aspect,
         Near          => 0.1,
         Far           => 1000.0);
   end Window_Resize;


   use type SDL.Events.Keyboards.Key_Codes;
   use type SDL.Events.Windows.Window_Event_ID;
   use type Windows.Window_Flags;
begin
   if SDL.Initialise then
      Video.GL.Set_Context_Profile (Video.GL.Core);
      Video.GL.Set_Core_Context_Profile (Major => 4, Minor => 6);

      Windows.Makers.Create
        (Win      => Prog_Window,
         Title    => Encoders.Encode ("Chapter 4: Simple Solar System"),
         Position => Windows.Centered_Window_Position,
         Size     => Window_Size,
         Flags    => Windows.OpenGL or Windows.Resizable);

      Video.GL.Create (Context, From => Prog_Window);
      Video.GL.Set_Current (Context, Prog_Window);
      Video.GL.Set_Swap_Interval (Video.GL.Synchronised);

      Initialise (Prog_Window);

      --  Display the contents, without this, the window is black.
      Window_Resize (Event.Window.ID, Window_Size);

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

               when SDL.Events.Windows.Window =>
                  if Event.Window.Event_ID = SDL.Events.Windows.Resized then
                     Window_Resize (Event.Window.ID,
                                    (Width  => SDL.Dimension (Event.Window.Data_1),
                                     Height => SDL.Dimension (Event.Window.Data_2)));
                  end if;

               when others =>
                  null;
            end case;
         end loop;

         Display (Prog_Window, Timers.Ticks);  --  In the book, they use glfwGetTime() which returns seconds.
         Video.GL.Swap (Prog_Window);

         exit Main when Finished;
      end loop Main;

      Windows.Finalize (Prog_Window);
      SDL.Quit;
   end if;
end Simple_Solar_System;