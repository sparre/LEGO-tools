------------------------------------------------------------------------------
--
--  procedure Build_MPD_File (body)
--
--  This program builds a multi-part dat (MPD) file based on the file named as
--  the command line argument. All referenced model files that can be found in
--  the current catalog will be added.
--
--  Command line arguments:
--    "-model" <Model file name> - Main file (.ldr or .dat is not neccessary).
--    "-path" <Directory-models> ... <Directory>
--                               - Directories where the MPD builder should
--                                 look for sub-models (optional).
--    "-overwrite"               - Don't check if the MPD file allready
--                                 exists (optional).
--    "-collect"                 - Don't store information about where the
--                                 sub-models are located.
--    "-collect" <Directory>     - Make all files appear in <Directory> when
--                                 the file is splitted.
--
------------------------------------------------------------------------------
--  Update information:
--
--  1999.02.11 (Jacob Sparre Andersen)
--    Written based on Split_LDraw_File (1997.06.20 version).
--
--  1999.09.29 (Jacob Sparre Andersen)
--    Added an "ad" for the MPD Builder to the resulting file.
--    Added the options "-path" and "-collect".
--
--  1999.09.30 (Jacob Sparre Andersen)
--    Terminates the file with a "NOFILE" comment.
--
--  2002.07.17 (Jacob Sparre Andersen)
--    Modified the file name handling, so both ".dat" and ".ldr" are
--      considered as valid extensions for LDraw files.
--    ".ldr" is now the default extension for LDraw files.
--
--  2002.07.31 (Jacob Sparre Andersen)
--    Modified the comment on where the software can be downloaded from.
--
--  2002.08.04 (Jacob Sparre Andersen)
--    Improved the help message.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

------------------------------------------------------------------------------
--  GNAT packages:
--
--  Used to get Directory_Separator.

with GNAT.OS_Lib;

------------------------------------------------------------------------------
--  Other packages:

with File_System;
with Generic_Command_Line_Processing;
with Generic_Command_Line_Types;
with List_Of_Static_Items_G;
with String_Arrays;
with UStrings;

------------------------------------------------------------------------------

procedure Build_MPD_File is

   ---------------------------------------------------------------------------
   --  Exceptions:

   Bad_LDraw_Command : exception;

   ---------------------------------------------------------------------------
   --  type Argument_Names:

   type Argument_Names is (Help, Model, Path, Overwrite, Collect);

   ---------------------------------------------------------------------------
   --  package Command_Line_Types:

   package Command_Line_Types is
     new Generic_Command_Line_Types (Argument_Names => Argument_Names);

   ---------------------------------------------------------------------------
   --  function U:

   function U (Item : in     String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   ---------------------------------------------------------------------------
   --  package Command_Line_Processing:

   package Command_Line_Processing is new Generic_Command_Line_Processing
     (Command_Line_Types  => Command_Line_Types,
      Obligatory          => (Model  => True,
                              others => False),
      Minimum_Field_Count => (Model  | Path => 1,
                              others        => 0),
      Maximum_Field_Count => (Model | Collect => 1,
                              Path            => Natural'Last,
                              others          => 0),
      Help                => (Model     => U (" <Model file name> - " &
                                                """.ldr"" or "".dat"" is " &
                                                "not necessary."),
                              Overwrite => U (" - Don't check if the MPD " &
                                                "file already exists."),
                              Path      => U (" <Directory> ... <Directory> " &
                                                "Directories where the MPD " &
                                                "builder should look for " &
                                                "sub-models."),
                              Collect   => U (" [ <Directory> ] - Make all " &
                                                "files appear in " &
                                                "<Directory> when the file " &
                                                "is splitted."),
                              others    => U (" - Show this message.")));

   ---------------------------------------------------------------------------
   --  package String_Lists:

   package String_Lists is new List_Of_Static_Items_G
     (Item_Type => Ada.Strings.Unbounded.Unbounded_String,
      Equals    => Ada.Strings.Unbounded."=");

   ---------------------------------------------------------------------------
   --  function Is_Meta_Command:

   function Is_Meta_Command (Line      : in     UStrings.UString;
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

   ---------------------------------------------------------------------------
   --  function Is_Subfile_Command:

   function Is_Subfile_Command (Line  : in     UStrings.UString)
     return Boolean is

      use Ada.Characters.Handling;
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use UStrings;

      Buffer : Unbounded_String;

   begin --  Is_Subfile_Command
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

   ---------------------------------------------------------------------------
   --  procedure Split_LDraw_Meta_Command:

   procedure Split_LDraw_Meta_Command (Line      : in     UStrings.UString;
                                       Command   :    out UStrings.UString;
                                       Arguments : in out UStrings.UString) is

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
   exception
      when Bad_LDraw_Command =>
         raise;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Build_MPD_File.Split_LDraw_Meta_Command: An " &
                    "unexpected exception occured. Aborting...");
         raise;
   end Split_LDraw_Meta_Command;

   ---------------------------------------------------------------------------
   --  procedure Extract_Subfile_Name:

   procedure Extract_Subfile_Name (Line         : in     UStrings.UString;
                                   Subfile_Name :    out UStrings.UString) is

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

   ---------------------------------------------------------------------------
   --  File lists:

   Scanned_Files     : String_Lists.List_Type;
   Unavailable_Files : String_Lists.List_Type;
   Unprocessed_Files : String_Lists.List_Type;

   ---------------------------------------------------------------------------
   --  procedure Scan_File:

   procedure Scan_File (File_Name : in     String) is

      use Ada.Text_IO;
      use String_Lists;
      use UStrings;

      Model        : File_Type;
      Current_Line : UString;
      Subfile_Name : UString;

   begin --  Scan_File
      Put (File => Current_Error,
           Item => "Scanning """ & File_Name & """... ");

      Open (File => Model,
            Name => File_Name,
            Mode => In_File);

      while not End_Of_File (File => Model) loop
         Get_Line (File => Model,
                   Item => Current_Line);

         if Is_Subfile_Command (Current_Line) then
            Extract_Subfile_Name (Line         => Current_Line,
                                  Subfile_Name => Subfile_Name);

            Insert (List => Unprocessed_Files,
                    Item => Subfile_Name);
         end if;
      end loop;

      Close (File => Model);

      Put_Line (File => Current_Error,
                Item => "Done.");
   exception
      when Bad_LDraw_Command =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Build_MPD_File.Scan_File: There is a bad LDraw " &
                    "command on line " & Count'Image (Line (Model)) &
                    " (or the preceding line). Skips to the next file.");

         Close (File => Model);

         return;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Build_MPD_File.Scan_File: An unexpected exception " &
                    "occured while processing the file " & File_Name &
                    ". Aborting...");
         raise;
   end Scan_File;

   ---------------------------------------------------------------------------

   use Ada.Characters.Handling;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Command_Line_Processing;
   use File_System;
   use String_Arrays;
   use String_Lists;
   use UStrings;

   Model_Name         : UString;
   DAT_Name, MPD_Name : UString;
   DAT_File, MPD_File : File_Type;

   Current_File : File_Type;
   Current_Line : UString;

   Is_Scanned, Is_Unavailable : Boolean;

   Paths : String_Array_Reference;

   Full_File_Name   : UString;
   Found_File       : Boolean;
   Stored_File_Name : UString;

begin --  Build_MPD_File
   if Set (Help) or not Set (Model) then
      Put_Help (File => Standard_Output);
   else
      if Set (Path) then
         Paths := new String_Array_Type (1 .. Field_Count (Path) + 1);

         Paths (1) := U ("");
         for Index in 2 .. Paths'Last loop
            Paths (Index) := U (Value (Path, Index - 1));

            if Element (Paths (Index), Length (Paths (Index)))
                 = GNAT.OS_Lib.Directory_Separator then
               null;
            else
               Append (Source   => Paths (Index),
                       New_Item => GNAT.OS_Lib.Directory_Separator);
            end if;
         end loop;
      else
         Paths := new String_Array_Type (1 .. 1);

         Paths (1) := U ("");
      end if;

      Model_Name := U (To_Lower (Value (Model, 1)));

      if Index (Model_Name, ".dat") > 0 then
         Delete (Source  => Model_Name,
                 From    => Index (Model_Name, ".dat"),
                 Through => Length (Model_Name));
         DAT_Name := Model_Name & ".dat";
      elsif Index (Model_Name, ".ldr") > 0 then
         Delete (Source  => Model_Name,
                 From    => Index (Model_Name, ".ldr"),
                 Through => Length (Model_Name));
         DAT_Name := Model_Name & ".ldr";
      else
         DAT_Name := Model_Name & ".ldr";
      end if;

      MPD_Name := Model_Name & ".mpd";

      Put_Line (File => Current_Error,
                Item => "Collecting """ & S (DAT_Name) & """ and it's " &
                        " submodels into """ & S (MPD_Name) & """...");

      if Set (Overwrite) then
         Create (File => MPD_File,
                 Name => S (MPD_Name),
                 Mode => Out_File);
      elsif File_System.Exists (S (MPD_Name)) then
         Put_Line (File => Current_Error,
                   Item => "There is allready a file named """ &
                           S (MPD_Name) & """. Please use " &
                           "the -overwrite argument if you want to " &
                           "overwrite the existing file.");
         return;
      else
         Create (File => MPD_File,
                 Name => S (MPD_Name),
                 Mode => Out_File);
      end if;

      Put_Line (File => MPD_File,
                Item => "0 Created with the MPD Builder. Can be converted");
      Put_Line (File => MPD_File,
                Item => "0 to LDraw files using the MPD Splitter. Both can");
      Put_Line (File => MPD_File,
                Item => "0 be downloaded from:");
      Put_Line (File => MPD_File,
                Item => "0");
      Put_Line (File => MPD_File,
                Item => "0    http://jacob.sparre.dk/Ada/mpd_files/");
      Put_Line (File => MPD_File,
                Item => "0");
      Put_Line (File => MPD_File,
                Item => "L3P, LDLite and LDGLite can process MPD files");
      Put_Line (File => MPD_File,
                Item => "directly.");
      New_Line (File => MPD_File);

      Insert (List => Unprocessed_Files,
              Item => DAT_Name);

      Put_Line (File => Current_Error,
                Item => "Scanning LDraw files...");

   Scan_Unprocessed_Files:
      loop
         exit Scan_Unprocessed_Files when Is_Empty (Unprocessed_Files);

         Set_Cursor_At_Front (List => Unprocessed_Files);

      Remove_Scanned_Files:
         loop
            exit Remove_Scanned_Files
               when Is_Cursor_At_Rear (Unprocessed_Files);

            Find_And_Set_Cursor (List  => Scanned_Files,
                                 Item  => Get (Unprocessed_Files),
                                 Found => Is_Scanned);

            if Is_Scanned then
               Remove (List => Unprocessed_Files);
            else
               Move_Cursor_To_Next (List => Unprocessed_Files);
            end if;
         end loop Remove_Scanned_Files;

         Set_Cursor_At_Front (List => Unprocessed_Files);

      Remove_Unavailable_Files:
         loop
            exit Remove_Unavailable_Files
               when Is_Cursor_At_Rear (Unprocessed_Files);

            Find_And_Set_Cursor (List  => Unavailable_Files,
                                 Item  => Get (Unprocessed_Files),
                                 Found => Is_Unavailable);

            if Is_Unavailable then
               Remove (List => Unprocessed_Files);
            else
               Full_File_Name := Get (Unprocessed_Files);
               Find_File (File_Name => Full_File_Name,
                          Path      => Paths,
                          Found_It  => Found_File);

               if Found_File then
                  Move_Cursor_To_Next (List => Unprocessed_Files);
               else
                  Insert (List => Unavailable_Files,
                          Item => Get (Unprocessed_Files));
                  Remove (List => Unprocessed_Files);
               end if;
            end if;
         end loop Remove_Unavailable_Files;

         Set_Cursor_At_Front (List => Unprocessed_Files);

      Scan_Files:
         loop
            exit Scan_Files when Is_Cursor_At_Rear (Unprocessed_Files);

            DAT_Name := Get (List => Unprocessed_Files);
            Remove (List => Unprocessed_Files);

            Find_And_Set_Cursor (List  => Scanned_Files,
                                 Item  => DAT_Name,
                                 Found => Is_Scanned);

            if Is_Scanned then
               null;
            else
               Full_File_Name := DAT_Name;
               Find_File (File_Name => Full_File_Name,
                          Path      => Paths,
                          Found_It  => Found_File);

               if Found_File then
                  Scan_File (File_Name => S (Full_File_Name));
                  Insert (List => Scanned_Files,
                          Item => DAT_Name);
               else
                  Insert (List => Unavailable_Files,
                          Item => DAT_Name);
               end if;
            end if;
         end loop Scan_Files;
      end loop Scan_Unprocessed_Files;

      Put_Line (File => Current_Error,
                Item => "Collecting the LDraw files...");

      Set_Cursor_At_Front (List => Scanned_Files);

   Collect_Files:
      while not Is_Empty (Scanned_Files) loop
         DAT_Name := Get (Scanned_Files);
         Remove (Scanned_Files);

         Full_File_Name := DAT_Name;
         Find_File (File_Name => Full_File_Name,
                    Path      => Paths,
                    Found_It  => Found_File);

         if Found_File then
            if Set (Collect) then
               if Field_Count (Collect) = 0 then
                  Stored_File_Name := DAT_Name;
               else
                  Stored_File_Name := Value (Collect, 1) & DAT_Name;
               end if;
            else
               Stored_File_Name := Full_File_Name;
            end if;

            Put_Line (File => Current_Error,
                      Item => "Adding """ & S (Full_File_Name) & """ to " &
                              "the MPD file as """ & S (Stored_File_Name) &
                              """.");

            Put_Line (File => MPD_File,
                      Item => "0 FILE " & S (Stored_File_Name));

            Open (File => DAT_File,
                  Name => S (Full_File_Name),
                  Mode => In_File);

            while not End_Of_File (File => DAT_File) loop
               Get_Line (File => DAT_File,
                         Item => Current_Line);

               if Is_Meta_Command (Line    => Current_Line,
                                   Command => "File") then
                  Put_Line (File => MPD_File,
                            Item => "0 Was: " & S (Current_Line));
               else
                  Put_Line (File => MPD_File,
                            Item => S (Current_Line));
               end if;
            end loop;

            Close (File => DAT_File);
         end if;
      end loop Collect_Files;

      Put_Line (File => MPD_File,
                Item => "0 NOFILE");
      Close (File => MPD_File);

      Put_Line (File => Current_Error,
                Item => "Done.");
   end if;
exception
   when Bad_LDraw_Command =>
      Put_Line (File => Current_Error,
                Item => "There appears to be a bug in one of the source " &
                        "files.");
   when others =>
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Current_Error,
         Item => "Build_MPD_File: An undocumented exception was raised. " &
                 "Please contact the author of the program with details of " &
                 "what happened. Aborting ...");
         raise;
end Build_MPD_File;
