------------------------------------------------------------------------------
--
--  package String_Arrays (spec)
--
--  Procedures to handle arrays of strings (of type
--  Ada.Strings.Unbounded.Unbounded_String).
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.08.01 (Jacob Sparre Andersen)
--    Written.
--
--  1997.10.05 (Jacob Sparre Andersen)
--    Added procedure Append (for a single string).
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--  Standardbiblioteker:

with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

------------------------------------------------------------------------------

package String_Arrays is

   ---------------------------------------------------------------------------
   --  type String_Array_Type:

   type String_Array_Type is array (Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;
   subtype Instance is String_Array_Type;

   ---------------------------------------------------------------------------
   --  type String_Array_Reference:

   type String_Array_Reference is access all String_Array_Type;
   subtype Reference is String_Array_Reference;

   ---------------------------------------------------------------------------
   --  procedure Split:
   --
   --  Splits a string into an array of words:
   --
   --  Exceptions:
   --    Constraint_Error - if Word_Separators is empty.

   procedure Split
     (Source                 : in     String;
      Word_Separators        : in     Ada.Strings.Maps.Character_Set;
      Target                 :    out String_Array_Reference;
      Skip_Repeat_Separators : in     Boolean := True);

   procedure Split
     (Source                 : in     Ada.Strings.Unbounded.Unbounded_String;
      Word_Separators        : in     Ada.Strings.Maps.Character_Set;
      Target                 :    out String_Array_Reference;
      Skip_Repeat_Separators : in     Boolean := True);

   ---------------------------------------------------------------------------
   --  function Split:
   --
   --  Splits a string into an array of words:
   --
   --  Exceptions:
   --    Constraint_Error - if Word_Separators is empty.

   function Split
     (Source                 : in     String;
      Word_Separators        : in     Ada.Strings.Maps.Character_Set;
      Skip_Repeat_Separators : in     Boolean := True)
     return String_Array_Type;

   ---------------------------------------------------------------------------
   --  procedure Append:
   --
   --  Splits the string Source into an array of words and appends it to
   --  Target:
   --
   --  Exceptions:
   --    Constraint_Error - if Word_Separators is empty.

   procedure Append (Source          : in     String;
                     Word_Separators : in     Ada.Strings.Maps.Character_Set;
                     Target          : in out String_Array_Reference);

   procedure Append
     (Source          : in     Ada.Strings.Unbounded.Unbounded_String;
      Word_Separators : in     Ada.Strings.Maps.Character_Set;
      Target          : in out String_Array_Reference);

   ---------------------------------------------------------------------------
   --  procedure Append:
   --
   --  Appends the string Source to Target:
   --
   --  Exceptions:
   --    (none)

   procedure Append (Source : in     String;
                     Target : in out String_Array_Reference);

   procedure Append (Source : in     Ada.Strings.Unbounded.Unbounded_String;
                     Target : in out String_Array_Reference);

   ---------------------------------------------------------------------------
   --  function Index:
   --
   --  Returns the index of the first occurring instance of Pattern in the
   --  list. The result is 0 (zero) if Pattern does not occur in the list.
   --
   --  Exceptions: (none)

   function Index (Source  : in     String_Array_Type;
                   Pattern : in     String) return Natural;

   function Index (Source  : in     String_Array_Type;
                   Pattern : in     Ada.Strings.Unbounded.Unbounded_String)
     return Natural;

   ---------------------------------------------------------------------------
   --  function Is_Subset:
   --
   --  Checks if all of Elements can be found in Set.

   function Is_Subset (Elements : in     String_Array_Type;
                       Set      : in     String_Array_Type) return Boolean;

   ---------------------------------------------------------------------------
   --  function "<=":
   --
   --  Equivalent to Is_Subset:

   function "<=" (Left  : in     String_Array_Type;
                  Right : in     String_Array_Type) return Boolean
     renames Is_Subset;

   ---------------------------------------------------------------------------
   --  procedure Remove_Subset:
   --
   --  Removes all elements in Elements from Set.
   --
   --  Exception:
   --    Constraint_Error - if Elements is not a subset of Set.

   procedure Remove_Subset (Elements      : in     String_Array_Type;
                            Set           : in out String_Array_Reference;
                            All_instances : in     Boolean := False);

   ---------------------------------------------------------------------------

end String_Arrays;
