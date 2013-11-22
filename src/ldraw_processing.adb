with
  Ada.Characters.Handling,
  Ada.Strings.Fixed,
  Ada.Text_IO;

package body LDraw_Processing is

   function Image (Item : in     File_Name_Extensions) return String;

   function Tail_Match (Item, Tail : in     String) return Boolean;

   ----------------------------------------------------------------------------

   procedure Extract_Subfile_Name
     (Line         : in     Ada.Strings.Unbounded.Unbounded_String;
      Subfile_Name :    out Ada.Strings.Unbounded.Unbounded_String) is

      use Ada.Characters.Handling;
      use Ada.Strings;
      use Ada.Strings.Unbounded;

      Buffer    : Unbounded_String;
      Separator : Natural;

   begin --  Extract_Subfile_Name
      Buffer := Trim (Source => Line,
                      Side   => Both);

      if Slice (Source => Buffer,
                Low    => 1,
                High   => 2) = "1 " then
         Separator := Index (Source  => Buffer,
                             Pattern => " ",
                             Going   => Backward);
         Subfile_Name := Delete (Source  => Buffer,
                                 From    => 1,
                                 Through => Separator);
         Translate (Source  => Subfile_Name,
                    Mapping => To_Lower'Access);
      else
         raise Bad_LDraw_Command;
      end if;
   exception
      when Bad_LDraw_Command =>
         raise;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Build_MPD_File.Extract_Subfile_Name: An " &
                    "unexpected exception occured. Aborting...");
         raise;
   end Extract_Subfile_Name;

   function Image (Item : in     File_Name_Extensions) return String is
   begin
      return
        Ada.Characters.Handling.To_Lower (File_Name_Extensions'Image (Item));
   end Image;

   function Is_Meta_Command
     (Line      : in     Ada.Strings.Unbounded.Unbounded_String;
      Command   : in     String) return Boolean is

      use Ada.Characters.Handling;
      use Ada.Strings;
      use Ada.Strings.Unbounded;

      Buffer    : Unbounded_String;
      Separator : Natural;

   begin --  Is_Meta_Command
      Buffer := Trim (Source => Line,
                      Side   => Both);

      if Length (Buffer) < 3 then
         return False;
      elsif Slice (Source => Buffer,
                Low    => 1,
                High   => 2) = "0 " then
         Delete (Source  => Buffer,
                 From    => 1,
                 Through => 2);
         Separator := Index (Source  => Buffer & " ",
                             Pattern => " ");

         return
           To_Upper (Command) = To_Upper (Slice (Source => Buffer,
                                                 Low    => 1,
                                                 High   => Separator - 1));
      else
         return False;
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Build_MPD_File.Is_Meta_Command: An unexpected " &
                    "exception occured. Aborting...");
         raise;
   end Is_Meta_Command;

   function Is_Subfile_Command
     (Line  : in     Ada.Strings.Unbounded.Unbounded_String) return Boolean is

      use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Unbounded;

      Buffer : Unbounded_String;

   begin
      Buffer := Trim (Source => Line,
                      Side   => Both);

      if Length (Buffer) < 3 then
         return False;
      elsif Slice (Source => Buffer,
                   Low    => 1,
                   High   => 2) = "1 " then
         return True;
      else
         return False;
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Build_MPD_File.Is_Subfile_Command: An unexpected " &
                    "exception occured. Aborting...");
         raise;
   end Is_Subfile_Command;

   function Set_Extension
     (File_Name     : in     String;
      New_Extension : in     File_Name_Extensions) return String is
      use Ada.Characters.Handling, Ada.Strings.Fixed;
   begin
      for Extension in File_Name_Extensions loop
         if Tail_Match (File_Name, "." & Image (Extension)) then
            return File_Name (File_Name'First ..
                              File_Name'Last - Image (Extension)'Length) &
                   Image (New_Extension);
         end if;
      end loop;

      return File_Name & "." & Image (New_Extension);
   end Set_Extension;

   procedure Split_LDraw_Meta_Command
     (Line      : in     Ada.Strings.Unbounded.Unbounded_String;
      Command   :    out Ada.Strings.Unbounded.Unbounded_String;
      Arguments : in out Ada.Strings.Unbounded.Unbounded_String) is

      use Ada.Characters.Handling;
      use Ada.Strings;
      use Ada.Strings.Unbounded;

      Buffer    : Unbounded_String;
      Separator : Natural;

   begin --  Split_LDraw_Meta_Command
      Buffer := Trim (Source => Line,
                      Side   => Both);

      if Slice (Source => Buffer,
                Low    => 1,
                High   => 2) = "0 " then
         Delete (Source  => Buffer,
                 From    => 1,
                 Through => 2);
         Separator := Index (Source  => Buffer & " ",
                             Pattern => " ");

         Command := To_Unbounded_String (Slice (Source => Buffer,
                                                Low    => 1,
                                                High   => Separator - 1));
         Arguments := To_Unbounded_String (Slice (Source => Buffer,
                                                  Low    => Separator + 1,
                                                  High   => Length (Buffer)));
      else
         raise Bad_LDraw_Command;
      end if;
   end Split_LDraw_Meta_Command;

   function Tail_Match (Item, Tail : in     String) return Boolean is
      use Ada.Characters.Handling, Ada.Strings;
   begin
      return To_Lower (Tail) = To_Lower (Fixed.Tail (Item, Tail'Length));
   end Tail_Match;

end LDraw_Processing;
