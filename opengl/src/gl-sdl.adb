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

   procedure Initialise is
   begin
      Clear_Colour := Init_Clear_Colour;
      Clear        := Init_Clear;
   end Initialise;
end GL.SDL;
