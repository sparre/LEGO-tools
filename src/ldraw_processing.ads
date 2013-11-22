with
  Ada.Strings.Unbounded;

package LDraw_Processing is
   type File_Name_Extensions is (Dat, Ldr, LDraw, MPD);

   function Set_Extension
     (File_Name     : in     String;
      New_Extension : in     File_Name_Extensions) return String;
   --  Substitutes/adds a LDraw file name extension.

   Bad_LDraw_Command : exception;

   function Is_Meta_Command
     (Line      : in     Ada.Strings.Unbounded.Unbounded_String;
      Command   : in     String) return Boolean;

   procedure Split_LDraw_Meta_Command
     (Line      : in     Ada.Strings.Unbounded.Unbounded_String;
      Command   :    out Ada.Strings.Unbounded.Unbounded_String;
      Arguments : in out Ada.Strings.Unbounded.Unbounded_String);

   function Is_Subfile_Command
     (Line  : in     Ada.Strings.Unbounded.Unbounded_String) return Boolean;

   procedure Extract_Subfile_Name
     (Line         : in     Ada.Strings.Unbounded.Unbounded_String;
      Subfile_Name :    out Ada.Strings.Unbounded.Unbounded_String);

end LDraw_Processing;
