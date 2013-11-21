------------------------------------------------------------------------------
--
--  package Generic_Command_Line_Types (spec)
--
--  This package declares types used for command line processing.
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.03.18 (Jacob Sparre Andersen)
--    Written.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Strings.Unbounded;

------------------------------------------------------------------------------

generic

   type Argument_Names is (<>);

package Generic_Command_Line_Types is

   ---------------------------------------------------------------------------
   --  subtype Argument_Range:

   subtype Argument_Range is Argument_Names;

   ---------------------------------------------------------------------------
   --  type Boolean_Array:

   type Boolean_Array is array (Argument_Names) of Boolean;

   ---------------------------------------------------------------------------
   --  type Natural_Array:

   type Natural_Array is array (Argument_Names) of Natural;

   ---------------------------------------------------------------------------
   --  type Help_Array:

   type Help_Array is array (Argument_Names)
     of Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------

end Generic_Command_Line_Types;
