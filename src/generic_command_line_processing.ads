with
  Ada.Text_IO;
with
  Generic_Command_Line_Types,
  String_Arrays;

generic

   with package Command_Line_Types is new Generic_Command_Line_Types (<>);

   Obligatory          : Command_Line_Types.Boolean_Array;
   Minimum_Field_Count : Command_Line_Types.Natural_Array;
   Maximum_Field_Count : Command_Line_Types.Natural_Array;

   Help : Command_Line_Types.Help_Array;

   Check_Arguments : Boolean := True;
   Argument_Marker : Character := '-';

package Generic_Command_Line_Processing is

   ---------------------------------------------------------------------------
   --  Exceptions:

   Argument_Error : exception; --  In case of bad command line arguments.

   ---------------------------------------------------------------------------
   --  subtype Argument_Names:

   subtype Argument_Names is Command_Line_Types.Argument_Range;

   ---------------------------------------------------------------------------
   --  procedure Put_Help:
   --
   --  Writes a help message to the file.
   --
   --  Exceptions:
   --    Same as for Ada.Text_IO.Put_Line.

   procedure Put_Help (File : in     Ada.Text_IO.File_Type);

   ---------------------------------------------------------------------------
   --  function All_Arguments_Valid:
   --
   --  Checks if all the command line arguments are valid (according to
   --  Valid).
   --
   --  Exceptions:
   --    None

   function All_Arguments_Valid return Boolean;

   ---------------------------------------------------------------------------
   --  function Valid:
   --
   --  Checks if Argument is valid (according to Obligatory,
   --  Minimum_Field_Count, and Maximum_Field_Count).
   --
   --  Exceptions:
   --    None

   function Valid (Argument : Argument_Names) return Boolean;

   ---------------------------------------------------------------------------
   --  function Set:
   --
   --  Checks if Argument has been passed to the program.
   --
   --  Exceptions:
   --    None

   function Set (Argument : Argument_Names) return Boolean;

   ---------------------------------------------------------------------------
   --  function Field_Count:
   --
   --  Returns the number of fields passed along with Argument.
   --
   --  Exceptions:
   --    None

   function Field_Count (Argument : Argument_Names) return Natural;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument.
   --
   --  Exceptions:
   --    Argument_Error - if there isn't an Index field for Argument.

   function Value (Argument : Argument_Names;
                   Index    : Positive) return String;

   function Values (Argument : Argument_Names) return String_Arrays.Instance;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument (or Default).
   --
   --  Exceptions:
   --    None

   function Value (Argument : Argument_Names;
                   Default  : String;
                   Index    : Positive) return String;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument.
   --
   --  Exceptions:
   --    Argument_Error - if there isn't an Index field for Argument or
   --                     if the field isn't an Integer.

   function Value (Argument : Argument_Names;
                   Index    : Positive) return Integer;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument (or Default).
   --
   --  Exceptions:
   --    Argument_Error - if the field isn't an Integer.

   function Value (Argument : Argument_Names;
                   Default  : Integer;
                   Index    : Positive) return Integer;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument.
   --
   --  Exceptions:
   --    Argument_Error - if there isn't an Index field for Argument or
   --                     if the field isn't an Float.

   function Value (Argument : Argument_Names;
                   Index    : Positive) return Float;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Returns the Index field for Argument (or Default).
   --
   --  Exceptions:
   --    Argument_Error - if the field isn't an Float.

   function Value (Argument : Argument_Names;
                   Default  : Float;
                   Index    : Positive) return Float;

   ---------------------------------------------------------------------------

end Generic_Command_Line_Processing;
