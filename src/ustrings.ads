with Text_IO, Ada.Strings.Unbounded;
use  Text_IO, Ada.Strings.Unbounded;

package UStrings is

  -- This package provides a simpler way to work with type
  -- Unbounded_String, since this type will be used very often.
  -- Most users will want to ALSO with "Ada.Strings.Unbounded".
  -- Ideally this would be a child package of "Ada.Strings.Unbounded".
  --

  -- This package provides the following simplifications:
  --  + Shortens the type name from "Unbounded_String" to "UString".
  --  + Creates shorter function names for To_Unbounded_String, i.e.
  --    To_UString(U) and U(S).  "U" is not a very readable name, but
  --    it's such a common operation that a short name seems appropriate
  --    (this function is needed every time a String constant is used).
  --    It also creates S(U) as the reverse of U(S).
  --  + Adds other subprograms, currently just "Swap".
  --  + Other packages can use this package to provide other simplifications.

  -- Developed by David A. Wheeler; released to the public domain.

  -- This version (C) 1995 Ada Resource Association, Columbus, Ohio.
  -- Permission is granted to use this program for any purpose,
  -- commercial or not, as long as credit is given to David A. Wheeler
  -- as the original developer.


  subtype UString is Unbounded_String;

  function To_UString(Source : String)  return Unbounded_String
                                         renames To_Unbounded_String;
  function U(Source : String)           return Unbounded_String
                                         renames To_Unbounded_String;
  function S(Source : Unbounded_String) return String
                                         renames To_String;

  -- "Swap" is important for reuse in some other packages, so we'll define it.

  procedure Swap(Left, Right : in out Unbounded_String);


  function Empty(S : Unbounded_String) return Boolean;
   -- returns True if Length(S)=0.
  pragma Inline(Empty);


  -- I/O Routines.
  procedure Get_Line(File : in File_Type; Item : out Unbounded_String);
  procedure Get_Line(Item : out Unbounded_String);

  procedure Put(File : in File_Type; Item : in Unbounded_String);
  procedure Put(Item : in Unbounded_String);

  procedure Put_Line(File : in File_Type; Item : in Unbounded_String);
  procedure Put_Line(Item : in Unbounded_String);

end UStrings;
