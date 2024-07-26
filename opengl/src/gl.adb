package body GL is
   overriding
   procedure Finalize (Self : in out Shader_Programs) is
   begin
      for I in Self.Program'Range loop
         C.Strings.Free (Self.Program (I));
      end loop;
   end Finalize;


   function Convert (Shader_Program : Shader_Program_Array) return Shader_Programs is
   begin
      return S : Shader_Programs (Shader_Program'Length) do
         for I in Shader_Program'Range loop
            S.Program (I)        := C.Strings.New_Char_Array (C.To_C (US.To_String (Shader_Program (I))));
            S.String_Lengths (I) := Int (US.Length (Shader_Program (I)));
         end loop;
      end return;
   end Convert;


   function Convert (Shader_Program : String) return Shader_Programs is
   begin
      return S : Shader_Programs (1) do
         S.Program (1)        := C.Strings.New_Char_Array (C.To_C (Shader_Program));
         S.String_Lengths (1) := Int (Shader_Program'Length);
      end return;
   end Convert;
end GL;
