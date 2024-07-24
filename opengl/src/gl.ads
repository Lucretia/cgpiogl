with Interfaces.C;

package GL is
   package C renames Interfaces.C;

   type Float32 is new C.C_float;

--  typedef void (APIENTRYP PFNGLCLEARCOLORPROC) (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
--  GLAPI void APIENTRY glClearColor (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
   type Clear_Colour_Ptr is access procedure (Red, Green, Blue, Alpha : Float32) with
     Convention => C;

   Clear_Colour : Clear_Colour_Ptr := null;

--  #define GL_DEPTH_BUFFER_BIT               0x00000100
--  #define GL_STENCIL_BUFFER_BIT             0x00000400
--  #define GL_COLOR_BUFFER_BIT               0x00004000
   type Clear_Buffer_Mask is new C.int;

   Depth_Buffer_Bit   : constant Clear_Buffer_Mask := 16#0000_0100#;
   Stencil_Buffer_Bit : constant Clear_Buffer_Mask := 16#0000_0400#;
   Color_Buffer_Bit   : constant Clear_Buffer_Mask := 16#0000_4000#;

--  typedef void (APIENTRYP PFNGLCLEARPROC) (GLbitfield mask);
--  GLAPI void APIENTRY glClear (GLbitfield mask);
   type Clear_Ptr is access procedure (Mask : Clear_Buffer_Mask) with
     Convention => C;

   Clear : Clear_Ptr := null;
end GL;
