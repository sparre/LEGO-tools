------------------------------------------------------------------------------
--
--  procedure Split_LDraw_File (body)
--
--  This program reads a multi-part LDraw file from standard input or from a
--  named file, and creates the files defined in the multi-part file.
--
--  Command line arguments:
--    -mpd <MPD File>                  - MPD file to split. (optional)
--    -overwrite                       - Don't check if the files allready
--                                       exists. (optional)
--    -show_comments                   - Show comments. (optional)
--    -create_directories ( yes | no ) - Create or don't new new directories
--                                       as they are needed. Default is "yes".
--                                       (optional)
--    -preserve_case ( yes | no )      - Preserve the case of file names.
--                                       Default is to convert file names to
--                                       lower case ("no"). (optional)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.06.20 (Jacob Sparre Andersen)
--    Written.
--
--  1999.09.29 (Jacob Sparre Andersen)
--    Added the "-mpd" command line argument.
--    Added support for "NOFILE" for marking data that shouldn't be stored.
--    Creates directories where required.
--    All file and directory names are tranlated to lower case.
--    Added the flag "-show_comments". Default is now to ignore everything
--      that isn't written to a DAT file.
--
--  2002.08.06 (Jacob Sparre Andersen)
--    Added the "-create_directories" command line argument for specifying
--      if the program should create new directories or not.
--    Added the "-preserve_case" command line argument for specifying if the
--      program should convert file names to lower case or not.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

------------------------------------------------------------------------------
--  GNAT specific packages:
--
--  Used to get Is_Directory and Directory_Separator.

with GNAT.OS_Lib;

------------------------------------------------------------------------------
--  Other packages:

with File_System;
with Generic_Command_Line_Processing;
with Generic_Command_Line_Types;
with OS.Make_Directory;
with UStrings;

------------------------------------------------------------------------------

procedure Split_LDraw_File is

   ---------------------------------------------------------------------------
   --  Exceptions:

   Bad_LDraw_Command : exception;
   Bad_File_Name     : exception;

   ---------------------------------------------------------------------------
   --  type Argument_Names:

   type Argument_Names is (Help, MPD, Overwrite, Show_Comments,
                           Create_Directories, Preserve_Case);

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
      Obligatory          => (others => False),
      Minimum_Field_Count => (Help | Overwrite | Show_Comments         => 0,
                              MPD | Create_Directories | Preserve_Case => 1),
      Maximum_Field_Count => (Help | Overwrite | Show_Comments         => 0,
                              MPD | Create_Directories | Preserve_Case => 1),
      Help                =>
        (MPD                => U (" <MPD File> - MPD file to split."),
         Overwrite          => U (" - Don't check if the files already " &
                                    "exist."),
         Show_Comments      => U (" - Show comments."),
         Create_Directories => U (" ( yes | no ) - Should new directories " &
                                    "be created as needed or not. Default " &
                                    "is ""yes""."),
         Preserve_Case      => U (" ( yes | no ) - Should the case of file " &
                                    "names be preserved or not. Default " &
                                    "is ""no"", which will convert file " &
                                    "names to lower case."),
         others             => U (" - Show this message.")));

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
   end Is_Meta_Command;

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
   end Split_LDraw_Meta_Command;

   ---------------------------------------------------------------------------
   --  procedure Lower_Case_Include_Command:

   Convert_File_Names : constant Boolean :=
     Command_Line_Processing.Value (Argument => Preserve_Case,
                                    Default  => "no",
                                    Index    => 1) = "no";

   procedure Lower_Case_Include_Command
     (Line : in out Ada.Strings.Unbounded.Unbounded_String) is

      use Ada.Characters.Handling;
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use UStrings;

   begin --  Lower_Case_Include_Command
      if Convert_File_Names then
         null;
      elsif Slice (Source => Trim (Source => Line,
                                   Side   => Left),
                   Low    => 1,
                   High   => 2) = "1 " then
         Line := U (To_Lower (S (Line)));
      end if;
   end Lower_Case_Include_Command;

   ---------------------------------------------------------------------------
   --  procedure Fix:
   --
   --  Swaps non-graphic characters with spaces, and adjusts the directory
   --  separators to the appropriate values ('/', ':' or '\') depending on the
   --  operating system.

   procedure Fix
     (File_Name : in out Ada.Strings.Unbounded.Unbounded_String) is

      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded;

   begin --  Fix
      for Index in 1 .. Length (File_Name) loop
         if Is_Control (Element (Source => File_Name,
                                 Index  => Index)) then
            Replace_Element (Source => File_Name,
                             Index  => Index,
                             By     => ' ');
         elsif Element (Source => File_Name,
                        Index  => Index) = '/' or else
               Element (Source => File_Name,
                        Index  => Index) = ':' or else
               Element (Source => File_Name,
                        Index  => Index) = '\' then
            Replace_Element (Source => File_Name,
                             Index  => Index,
                             By     => GNAT.OS_Lib.Directory_Separator);
         end if;
      end loop;
   end Fix;

   ---------------------------------------------------------------------------
   --  procedure Create_Path:
   --
   --  Creates the directories neccesary for the file, and adjusts the
   --  directory separators to the appropriate values ('/' or '\') depending
   --  on the operating system.

   procedure Create_Path (To : in     String) is

      use Ada.Characters.Handling;
      use GNAT.OS_Lib;

      Buffer : String := To;

   begin --  Create_Path
      for Index in Buffer'Range loop
         if Is_Control (Buffer (Index)) then
            Buffer (Index) := ' ';
         elsif Buffer (Index) = '/' or Buffer (Index) = '\' then
            Buffer (Index) := Directory_Separator;

            if Is_Directory (Buffer (Buffer'First .. Index - 1)) then
               null;
            else
               OS.Make_Directory (Buffer (Buffer'First .. Index - 1));
            end if;
         end if;
      end loop;
   end Create_Path;

   ---------------------------------------------------------------------------
   --  procedure Process_File:

   procedure Process_File
     (Source                     : in     Ada.Text_IO.File_Type;
      Overwrite                  : in     Boolean;
      Show_Comments              : in     Boolean;
      Create_Missing_Directories : in     Boolean) is

      use Ada.Strings.Maps.Constants;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use UStrings;

      Target       : File_Type;
      Current_Line : Unbounded_String;
      Command      : Unbounded_String;
      File_Name    : Unbounded_String;

   begin --  Process_File
      while not End_Of_File (Source) loop
         Get_Line (File => Source,
                   Item => Current_Line);

         if Is_Meta_Command (Line    => Current_Line,
                             Command => "NoFile") then
            if Is_Open (File => Target) then
               Close (File => Target);
            end if;
         elsif Is_Meta_Command (Line    => Current_Line,
                                Command => "File") then
            if Is_Open (File => Target) then
               Close (File => Target);
            end if;

            Split_LDraw_Meta_Command (Line      => Current_Line,
                                      Command   => Command,
                                      Arguments => File_Name);
            Translate (Source  => File_Name,
                       Mapping => Lower_Case_Map);
            Fix (File_Name => File_Name);

            if not Overwrite and File_System.Exists (S (File_Name)) then
               Put_Line (File => Current_Error,
                         Item => "There is allready a file named """ &
                                 S (File_Name) & """. The data are written " &
                                 "to standard output instead. Please use " &
                                 "the -overwrite argument if you want to " &
                                 "overwrite the existing file.");
            else
               begin
                  if Create_Missing_Directories then
                     Create_Path (To => S (File_Name));
                  end if;

               Handle_Bad_File_Names:
                  begin
                     Create (File => Target,
                             Name => S (File_Name),
                             Mode => Out_File);
                  exception
                     when others =>
                        Put_Line (File => Current_Error,
                                  Item => "Could not create a file named """ &
                                          S (File_Name) & """.");
                        raise Bad_File_Name;
                  end Handle_Bad_File_Names;

                  Put_Line (File => Target,
                            Item => "0 FILE " & S (File_Name));
                  Put_Line (File => Current_Error,
                            Item => "Writing LDraw file """ & S (File_Name) &
                                    """.");
               exception
                  when others =>
                     Put_Line (File => Current_Error,
                               Item => "Unable to create the file """ &
                                       S (File_Name) & """.");
               end;
            end if;
         elsif Is_Open (File => Target) then
            Put_Line (File => Target,
                      Item => Current_Line);
         elsif Show_Comments then
            Put_Line (File => Standard_Output,
                      Item => Current_Line);
         end if;
      end loop;

      if Is_Open (Target) then
         Close (File => Target);
      end if;
   end Process_File;

   ---------------------------------------------------------------------------

   use Ada.Text_IO;
   use Command_Line_Processing;

   Create_Missing_Directories : constant Boolean :=
                                  Value (Argument => Create_Directories,
                                         Default  => "yes",
                                         Index    => 1) = "yes";

   Source : File_Type;

begin --  Split_LDraw_File
   if Set (Help) then
      Put_Help (File => Standard_Output);
   elsif Set (MPD) then
      Put_Line (File => Current_Error,
                Item => "Reading multi-part LDraw file """ & Value (MPD, 1) &
                        """.");
      Open (File => Source,
            Name => Value (MPD, 1),
            Mode => In_File);
      Process_File (Source                     => Source,
                    Overwrite                  => Set (Overwrite),
                    Show_Comments              => Set (Show_Comments),
                    Create_Missing_Directories => Create_Missing_Directories);
      Close (File => Source);
   else
      Put_Line (File => Current_Error,
                Item => "Reading multi-part LDraw file from standard input.");
      Process_File (Source                     => Standard_Input,
                    Overwrite                  => Set (Overwrite),
                    Show_Comments              => Set (Show_Comments),
                    Create_Missing_Directories => Create_Missing_Directories);
   end if;
exception
   when Bad_File_Name =>
      return;
   when others =>
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Current_Error,
         Item => "Split_LDraw_File: An undocumented exception was raised. " &
                 "Aborting ...");
         raise;
end Split_LDraw_File;
