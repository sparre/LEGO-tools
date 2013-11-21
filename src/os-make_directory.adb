------------------------------------------------------------------------------
--
--  procedure OS.Make_Directory (body)
--
--  Calls the system command "mkdir".
--
--  Exceptions:
--    System_Error -
--
------------------------------------------------------------------------------
--  Update information:
--
--  1999.09.25 (Jacob Sparre Andersen)
--    Written. Partially based on OS.Send_Mail (1997.02.13 edition).
--  1999.10.10 (Jacob Sparre Andersen)
--    No exception is raised if the directory already exists.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Text_IO;
with Interfaces.C.Strings;

------------------------------------------------------------------------------
--  GNAT packages:

with GNAT.OS_Lib;

------------------------------------------------------------------------------

procedure OS.Make_Directory (Name : in     String) is

   use Interfaces.C;
   use Interfaces.C.Strings;

   function system (Command : chars_ptr) return Interfaces.C.int;
   pragma Import (C, system);

   Command_In_C_Format : chars_ptr := New_String ("mkdir " & Name);
   Error_Code          : Interfaces.C.int := system (Command_In_C_Format);

begin --  OS.Make_Directory
   Free (Command_In_C_Format);

   if Error_Code = 0 then
      null; --  No error.
   elsif Error_Code = 256 then
      --  The name is in use.

      if GNAT.OS_Lib.Is_Directory (Name) then
         null; --  No problem.
      else
         raise System_Error;
      end if;
   else
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Current_Error,
         Item => "OS.Make_Directory: system returned "  &
                 Interfaces.C.int'Image (Error_Code));
      raise System_Error;
   end if;
end OS.Make_Directory;
