with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
--  with Ada.Real_Time;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Timers;
with SDL.Video.GL;
with SDL.Video.Windows.Makers;
with GL.SDL;

procedure First is
   --  package RT renames Ada.Real_Time;
   package Encoders renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
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

   procedure Initialise (Window : Windows.Window) is
   begin
      GL.SDL.Initialise;  -- GL entry points.
   end Initialise;

   procedure Display (Window : Windows.Window; Current_Time_MS : Timers.Milliseconds_Long) is
   begin
      GL.Clear_Colour (Red => 1.0, Green => 0.0, Blue => 0.0, Alpha => 1.0);
      GL.Clear (GL.Color_Buffer_Bit);
   end Display;

   use type SDL.Events.Keyboards.Key_Codes;
begin
   if SDL.Initialise then
      Video.GL.Set_Context_Profile (Video.GL.Core);
      Video.GL.Set_Core_Context_Profile (Major => 4, Minor => 6);

      Windows.Makers.Create
        (Win      => Prog_Window,
         Title    => Encoders.Encode ("Chapter 2: Program 1"),
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

         exit when Finished;
      end loop Main;

      Windows.Finalize (Prog_Window);
      SDL.Quit;
   end if;
end First;