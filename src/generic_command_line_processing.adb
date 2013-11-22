------------------------------------------------------------------------------
--
--  package Generic_Command_Line_Processing (body)
--
--  This package is used to process command line arguments.
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.03.18 (Jacob Sparre Andersen)
--    Written.
--
--  1997.03.19 (Jacob Sparre Andersen)
--    Improved help and error processing.
--
--  1997.04.10 (Jacob Sparre Andersen)
--    Implemented function All_Arguments_Valid.
--    Recoded the package body to use function All_Arguments_Valid.
--
--  1998.06.26 (Jacob Sparre Andersen)
--    Observed an error in the package.
--
--  2001.08.10 (Jacob Sparre Andersen)
--    Extended the exception handling.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;

------------------------------------------------------------------------------

package body Generic_Command_Line_Processing is

   function To_String (Argument : in     Argument_Names) return String;

   function To_Argument (Item : in     String) return Argument_Names;

   function Is_An_Argument (Item : in     String) return Boolean;

   procedure Process_Arguments;

   ---------------------------------------------------------------------------
   --  Argument information:

   ---------------------------------------------------------------------
   --  Argument_Index:
   --
   --  0 means that the argument hasn't been found (yet).

   Argument_Index : Command_Line_Types.Natural_Array := (others => 0);

   ---------------------------------------------------------------------
   --  Argument_Field_Count:
   --
   --  A field count of zero is equivalent to a flag.

   Argument_Field_Count : Command_Line_Types.Natural_Array := (others => 0);

   ---------------------------------------------------------------------------
   --  function All_Arguments_Valid:
   --
   --  Checks if all the command line arguments are valid (according to
   --  Valid).
   --
   --  Exceptions:
   --    None

   function All_Arguments_Valid return Boolean is

      use Ada.Text_IO;

      OK : Boolean := True;

   begin --  All_Arguments_Valid
      for Argument in Argument_Names loop
         if Valid (Argument) then
            null; --  OK!
         else
            if OK then
               Put (File => Current_Error,
                    Item => "Error. Problems with the command line " &
                            "arguments: " & To_String (Argument));
            else
               Put (File => Current_Error,
                    Item => ", " & To_String (Argument));
            end if;

            OK := False;
         end if;
      end loop;

      if OK then
         null; --  OK!
      else
         Put_Line (File => Current_Error,
                   Item => ".");
      end if;

      return OK;
   end All_Arguments_Valid;

   ---------------------------------------------------------------------------
   --  function Field_Count:
   --
   --  Returns the number of fields passed along with Argument.
   --
   --  Exceptions:
   --    None

   function Field_Count (Argument : Argument_Names) return Natural is

   begin --  Field_Count
      return Argument_Field_Count (Argument);
   end Field_Count;

   pragma Inline (Field_Count);

   ---------------------------------------------------------------------------
   --  function Is_An_Argument (private):
   --
   --  Checks if Item is a valid command line argument.
   --
   --  Exceptions:
   --    None

   function Is_An_Argument (Item : in     String) return Boolean is

      Trimmed_Name : constant String := Item (Item'First + 1 .. Item'Last);

      Argument : Argument_Names;

   begin --  Is_An_Argument
      Argument := Argument_Names'Value (Trimmed_Name);

      return Argument'Valid;
   exception
      when Constraint_Error =>
         return False;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Generic_Command_Line_Processing.Is_An_Argument: An " &
                    "undocumented exception occured. Aborting ...");
         raise;
   end Is_An_Argument;

   ---------------------------------------------------------------------
   --  procedure Process_Arguments (private):

   procedure Process_Arguments is
      use Ada.Command_Line;

      Current_Argument : Argument_Names;
   begin
      if Argument_Count >= 1 and then Is_An_Argument (Argument (1)) then
         Current_Argument := To_Argument (Argument (1));

         Argument_Index       (Current_Argument) := 1;
         Argument_Field_Count (Current_Argument) := 0;

         for Index in 2 .. Argument_Count loop
            if Is_An_Argument (Argument (Index)) then
               Current_Argument := To_Argument (Argument (Index));

               Argument_Index       (Current_Argument) := Index;
               Argument_Field_Count (Current_Argument) := 0;
            else
               Argument_Field_Count (Current_Argument) :=
                 Argument_Field_Count (Current_Argument) + 1;
            end if;
         end loop;
      elsif Argument_Count >= 1 then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "The first command line argument is not one of the " &
                    "valid arguments.");
         Ada.Text_IO.New_Line (File => Ada.Text_IO.Current_Error);
         Put_Help (File => Ada.Text_IO.Current_Error);

         raise Argument_Error;
      end if;
   end Process_Arguments;

   ---------------------------------------------------------------------------
   --  procedure Put_Help:
   --
   --  Writes a help message to the file.
   --
   --  Exceptions:
   --    Same as for Ada.Text_IO.Put_Line.

   procedure Put_Help (File : in     Ada.Text_IO.File_Type) is

      use Ada.Command_Line;
      use Ada.Integer_Text_IO;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

   begin --  Put_Help
      Put_Line (File => File,
                Item => "Command line arguments for '" & Command_Name & "':");
      New_Line (File => File);

      for Argument in Argument_Names loop
         Put (File => File,
              Item => "   ");
         Put (File => File,
              Item => To_String (Argument));
         Put (File => File,
              Item => " " & To_String (Help (Argument)) & " [");

         if Obligatory (Argument) then
            Put (File => File,
                 Item => "obligatory");
         else
            Put (File => File,
                 Item => "optional");
         end if;

         if Maximum_Field_Count (Argument) = 0 then
            Put (File => File,
                 Item => " flag");
         elsif Minimum_Field_Count (Argument) =
               Maximum_Field_Count (Argument) then
            Put (File => File,
                 Item => ", exactly ");
            Put (File  => File,
                 Item  => Minimum_Field_Count (Argument),
                 Width => 0);

            if Minimum_Field_Count (Argument) = 1 then
               Put (File => File,
                    Item => " field");
            else
               Put (File => File,
                    Item => " fields");
            end if;
         else
            Put (File => File,
                 Item => ", ");
            Put (File  => File,
                 Item  => Minimum_Field_Count (Argument),
                 Width => 0);
            Put (File => File,
                 Item => " to ");
            Put (File  => File,
                 Item  => Maximum_Field_Count (Argument),
                 Width => 0);
            Put (File => File,
                 Item => " fields");
         end if;

         Put_Line (File => File,
                   Item => "]");
      end loop;

      New_Line (File => File);
   end Put_Help;

   ---------------------------------------------------------------------------
   --  function Set:
   --
   --  Checks if Argument has been passed to the program.
   --
   --  Exceptions:
   --    None

   function Set (Argument : Argument_Names) return Boolean is

   begin --  Set
      return Argument_Index (Argument) > 0;
   end Set;

   pragma Inline (Set);

   ---------------------------------------------------------------------------
   --  function To_Argument (private):
   --
   --  Returns Item as a command line argument.
   --
   --  Exceptions:
   --    Constraint_Error - if Item isn't a valid command line argument.

   function To_Argument (Item : in     String) return Argument_Names is

      Trimmed_Name : constant String := Item (Item'First + 1 .. Item'Last);

   begin --  To_Argument
      return Argument_Names'Value (Trimmed_Name);
   end To_Argument;

   ---------------------------------------------------------------------------
   --  function To_String (private):
   --
   --  Returns Argument as a string.
   --
   --  Exceptions:
   --    None

   function To_String (Argument : in     Argument_Names) return String is

      use Ada.Characters.Handling;

   begin --  To_String
      return Argument_Marker & To_Lower (Argument_Names'Image (Argument));
   end To_String;

   ---------------------------------------------------------------------------
   --  function Valid:
   --
   --  Checks if Argument is valid (according to Obligatory,
   --  Minimum_Field_Count, and Maximum_Field_Count).
   --
   --  Exceptions:
   --    None

   function Valid (Argument : Argument_Names) return Boolean is

      subtype Valid_Field_Count_Range is Natural range
        Minimum_Field_Count (Argument) .. Maximum_Field_Count (Argument);

   begin
      if Set (Argument) then
         return Field_Count (Argument) in Valid_Field_Count_Range;
      else
         return not Obligatory (Argument);
      end if;
   end Valid;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument.
   --
   --  Exceptions:
   --    Argument_Error - if there isn't an Index field for Argument.

   function Value (Argument : Argument_Names;
                   Index    : Positive) return String is

   begin --  Value
      if Index <= Field_Count (Argument) then
         return Ada.Command_Line.Argument (Argument_Index (Argument) + Index);
      else
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: Asked for field number " &
                    Positive'Image (Index) & " for the argument '" &
                    To_String (Argument) & "'. Only " &
                    Positive'Image (Field_Count (Argument)) & " were " &
                    "passed to the program.");
         Ada.Text_IO.New_Line (File => Ada.Text_IO.Current_Error);
         Put_Help (File => Ada.Text_IO.Current_Error);

         raise Argument_Error;
      end if;
   end Value;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument (or Default).
   --
   --  Exceptions:
   --    None

   function Value (Argument : Argument_Names;
                   Default  : String;
                   Index    : Positive) return String is

   begin --  Value
      if Index <= Field_Count (Argument) then
         return Ada.Command_Line.Argument (Argument_Index (Argument) + Index);
      else
         return Default;
      end if;
   end Value;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument.
   --
   --  Exceptions:
   --    Argument_Error - if there isn't an Index field for Argument or
   --                     if the field isn't an Integer.

   function Value (Argument : Argument_Names;
                   Index    : Positive) return Integer is

   begin --  Value
      return Integer'Value (Value (Argument, Index));
   exception
      when Argument_Error =>
         raise; --  Propagate the exception from the String version of Value.
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: Field number " & Positive'Image (Index) & " for" &
                    " the argument '" & To_String (Argument) & "' should be" &
                    " an integer.");
         Ada.Text_IO.New_Line (File => Ada.Text_IO.Current_Error);
         Put_Help (File => Ada.Text_IO.Current_Error);

         raise Argument_Error;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: An undocumented exception occured. Aborting ...");
         raise;
   end Value;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument (or Default).
   --
   --  Exceptions:
   --    Argument_Error - if the field isn't an Integer.

   function Value (Argument : Argument_Names;
                   Default  : Integer;
                   Index    : Positive) return Integer is

   begin --  Value
      return Integer'Value (Value (Argument, Integer'Image (Default), Index));
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: Field number " & Positive'Image (Index) & " for" &
                    " the argument '" & To_String (Argument) & "' should be" &
                    " an integer.");
         Ada.Text_IO.New_Line (File => Ada.Text_IO.Current_Error);
         Put_Help (File => Ada.Text_IO.Current_Error);

         raise Argument_Error;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: An undocumented exception occured. Aborting ...");
         raise;
   end Value;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument.
   --
   --  Exceptions:
   --    Argument_Error - if there isn't an Index field for Argument or
   --                     if the field isn't an Float.

   function Value (Argument : Argument_Names;
                   Index    : Positive) return Float is

   begin --  Value
      return Float'Value (Value (Argument, Index));
   exception
      when Argument_Error =>
         raise; --  Propagate the exception from the String version of Value.
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: Field number " & Positive'Image (Index) & " for" &
                    " the argument '" & To_String (Argument) & "' should be" &
                    " an integer.");
         Ada.Text_IO.New_Line (File => Ada.Text_IO.Current_Error);
         Put_Help (File => Ada.Text_IO.Current_Error);

         raise Argument_Error;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: An undocumented exception occured. Aborting ...");
         raise;
   end Value;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument (or Default).
   --
   --  Exceptions:
   --    Argument_Error - if the field isn't an Float.

   function Value (Argument : Argument_Names;
                   Default  : Float;
                   Index    : Positive) return Float is

   begin --  Value
      return Float'Value (Value (Argument, Float'Image (Default), Index));
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: Field number " & Positive'Image (Index) & " for" &
                    " the argument '" & To_String (Argument) & "' should be" &
                    " an integer.");
         Ada.Text_IO.New_Line (File => Ada.Text_IO.Current_Error);
         Put_Help (File => Ada.Text_IO.Current_Error);

         raise Argument_Error;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Error: An undocumented exception occured. Aborting ...");
         raise;
   end Value;

   function Values (Argument : Argument_Names) return String_Arrays.Instance is
   begin
      return Result : String_Arrays.Instance
                        (1 .. Argument_Field_Count (Argument))
      do
         for I in Result'Range loop
            Result (I) := Ada.Strings.Unbounded.To_Unbounded_String
                            (String'(Value (Argument, I)));
         end loop;
      end return;
   end Values;

   ---------------------------------------------------------------------------

begin --  Generic_Command_Line_Processing
   Process_Arguments;

   if Check_Arguments then
      if All_Arguments_Valid then
         null; --  OK!
      else
         Ada.Text_IO.New_Line (File => Ada.Text_IO.Current_Error);
         Put_Help (File => Ada.Text_IO.Current_Error);

         raise Argument_Error;
      end if;
   end if;
exception
   when Argument_Error =>
      raise;
   when others =>
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Current_Error,
         Item => "Generic_Command_Line_Processing: An undocumented " &
                 "exception occured. Aborting ...");
      raise;
end Generic_Command_Line_Processing;
