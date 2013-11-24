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
--
------------------------------------------------------------------------------

with
  Ada.Command_Line,
  Ada.Strings.Unbounded,
  Ada.Text_IO,
  Ada.Text_IO.Unbounded_IO;
with
  File_System,
  Generic_Command_Line_Processing,
  Generic_Command_Line_Types,
  LDraw_Processing,
  String_Arrays,
  String_Sets;

------------------------------------------------------------------------------

procedure Build_MPD_File is

   procedure Scan_And_Copy_Model
     (Name                 : in     String;
      Included_Directories : in     String_Arrays.Instance;
      Target               : in     Ada.Text_IO.File_Type;
      Seen                 : in out String_Sets.Set;
      Scanned              : in out String_Sets.Set;
      Not_Found            : in out String_Sets.Set);

   procedure Process (Model                : in     String;
                      Included_Directories : in     String_Arrays.Instance);

   procedure Put_Header (File : in     Ada.Text_IO.File_Type);
   procedure Put_Tail   (File : in     Ada.Text_IO.File_Type);

   function Current_Directory return String_Arrays.Instance;

   ---------------------------------------------------------------------------

   function Current_Directory return String_Arrays.Instance is
   begin
      return (1 => Ada.Strings.Unbounded.To_Unbounded_String ("."));
   end Current_Directory;

   procedure Process (Model                : in     String;
                      Included_Directories : in     String_Arrays.Instance) is
      use Ada.Text_IO;
      use LDraw_Processing;
      use type String_Sets.Set;

      Seen, Scanned, Not_Found : String_Sets.Set;
      Target                   : Ada.Text_IO.File_Type;
   begin
      Put_Line (File => Current_Error,
                Item => "Collecting """ & Model & """ and it's " &
                        " submodels into """ & Set_Extension (Model, MPD) &
                        """...");

      Create (File => Target,
              Name => Set_Extension (Model, MPD),
              Mode => Out_File);
      Put_Header (File => Target);

      Scan_And_Copy_Model (Name                 => Model,
                           Included_Directories => Included_Directories,
                           Target               => Target,
                           Seen                 => Seen,
                           Scanned              => Scanned,
                           Not_Found            => Not_Found);

      while not Seen.Is_Empty loop
         Seen := Seen - Scanned;
         Seen := Seen - Not_Found;

         exit when Seen.Is_Empty;

         declare
            To_Process : constant String_Sets.Set := Seen;
         begin
            for Sub_Model of To_Process loop
               Scan_And_Copy_Model
                 (Name                 => Model,
                  Included_Directories => Included_Directories,
                  Target               => Target,
                  Seen                 => Seen,
                  Scanned              => Scanned,
                  Not_Found            => Not_Found);
            end loop;
         end;
      end loop;

      Put_Tail (File => Target);
      Close (File => Target);
   end Process;

   procedure Put_Header (File : in     Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
   begin
      Put_Line (File, "0 Created with the MPD Builder. Can be converted");
      Put_Line (File, "0 to LDraw files using the MPD Splitter. Both can");
      Put_Line (File, "0 be downloaded from:");
      Put_Line (File, "0");
      Put_Line (File, "0    http://jacob.sparre.dk/Ada/mpd_files/");
      Put_Line (File, "0");
      Put_Line (File, "0 L3P, LDLite and LDGLite can process MPD files");
      Put_Line (File, "0 directly.");
      New_Line (File);
   end Put_Header;

   procedure Put_Tail (File : in     Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
   begin
      Put_Line (File, "0 NOFILE");
   end Put_Tail;

   procedure Scan_And_Copy_Model
     (Name                 : in     String;
      Included_Directories : in     String_Arrays.Instance;
      Target               : in     Ada.Text_IO.File_Type;
      Seen                 : in out String_Sets.Set;
      Scanned              : in out String_Sets.Set;
      Not_Found            : in out String_Sets.Set) is

      use Ada.Strings.Unbounded;
      use File_System;

      File_Name  : Unbounded_String := To_Unbounded_String (Name);
      Found_File : Boolean;
   begin
      Find_File (File_Name => File_Name,
                 Path      => Included_Directories,
                 Found_It  => Found_File);

      if Found_File then
         declare
            use Ada.Text_IO, Ada.Text_IO.Unbounded_IO;
            use LDraw_Processing;
            Model        : Ada.Text_IO.File_Type;
            Current_Line : Unbounded_String;
            Subfile_Name : Unbounded_String;
         begin
            Put (File => Current_Error,
                 Item => "Scanning """ & File_Name & """... ");

            Open (File => Model,
                  Name => To_String (File_Name),
                  Mode => In_File);

            Put_Line (File => Target,
                      Item => "0 FILE " & File_Name);

            while not End_Of_File (File => Model) loop
               Get_Line (File => Model,
                         Item => Current_Line);

               if Is_Meta_Command (Line    => Current_Line,
                                   Command => "File") then
                  Put_Line (File => Target,
                            Item => "0 Was: " & Current_Line);
               else
                  Put_Line (File => Target,
                            Item => Current_Line);
               end if;

               if Is_Subfile_Command (Current_Line) then
                  Extract_Subfile_Name (Line         => Current_Line,
                                        Subfile_Name => Subfile_Name);

                  Ada.Text_IO.Put_Line
                    ("Inserting '" & To_String (Subfile_Name) &
                      "' in Seen.");
                  Seen.Insert (To_String (Subfile_Name));
               end if;
            end loop;

            Put_Line ("Inserting '" & Name & "' in Scanned.");
            Scanned.Insert (Name);

            Put_Line (File => Current_Error,
                      Item => "Done.");
         exception
            when End_Error =>
               Put_Line ("Inserting '" & Name & "' in Scanned.");
               Scanned.Insert (Name);

               Put_Line (File => Current_Error,
                         Item => "Done.");
            when Bad_LDraw_Command =>
               Put_Line
                 (File => Current_Error,
                  Item => "There is a bad LDraw command on line " &
                          Ada.Text_IO.Count'Image (Line (Model)) &
                          " (or the preceding line) in """ & File_Name &
                          """. Skips to the next file.");
               return;
            when others =>
               Put_Line
                 (File => Current_Error,
                  Item => "An unexpected exception " &
                          "occured while processing the file " & File_Name &
                          ". Aborting...");
               raise;
         end;
      else
         Ada.Text_IO.Put_Line ("Inserting '" & Name & "' in Not_Found.");
         Not_Found.Insert (Name);
      end if;
   end Scan_And_Copy_Model;

   ---------------------------------------------------------------------------

   use Ada.Command_Line, Ada.Text_IO;
begin
   declare
      type Argument_Names is (Help, Model, Path);

      package Command_Line_Types is
         new Generic_Command_Line_Types (Argument_Names => Argument_Names);

      function U (Item : in     String)
                 return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      package Command_Line_Processing is new Generic_Command_Line_Processing
        (Command_Line_Types  => Command_Line_Types,
         Obligatory          => (Model  => True,
                                 others => False),
         Minimum_Field_Count => (Model  | Path => 1,
                                 others        => 0),
         Maximum_Field_Count => (Model  => 1,
                                 Path   => Natural'Last,
                                 others => 0),
         Help                =>
           (Model     => U (" <Model file name> - " &
                              """.ldr"" or "".dat"" is " &
                              "not necessary."),
            Path      => U (" <Directory> ... <Directory> " &
                              "Directories where the MPD " &
                              "builder should look for " &
                              "sub-models."),
            others    => U (" - Show this message.")));

      use Command_Line_Processing;
   begin
      if Set (Help) then
         Put_Help (File => Standard_Output);
      elsif Set (Model) then
         if Set (Path) then
            Process (Model                => Value (Model, 1),
                     Included_Directories => Values (Path));
         else
            Process (Model                => Value (Model, 1),
                     Included_Directories => Current_Directory);
         end if;
      else
         Put_Help (File => Standard_Error);
         Set_Exit_Status (Failure);
      end if;
   end;
exception
   when LDraw_Processing.Bad_LDraw_Command =>
      Put_Line (File => Current_Error,
                Item => "There appears to be a bug in one of the source " &
                        "files.");
      Set_Exit_Status (Failure);
   when others =>
      Put_Line (File => Current_Error,
                Item => "An undocumented exception was raised. " &
                  "Please contact the author of the program with details of " &
                  "what happened. Aborting ...");
      raise;
end Build_MPD_File;
