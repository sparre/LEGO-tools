------------------------------------------------------------------------------
--
--  package File_System (spec)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1996.05.16 (Jacob Sparre Andersen)
--    Written.
--
--  1997.08.07 (Jacob Sparre Andersen)
--    Added procedure Copy.
--
--  1997.08.14 (Jacob Sparre Andersen)
--    Added procedure Delete.
--
--  1997.08.21 (Jacob Sparre Andersen)
--    Added function Size.
--
--  1999.09.15 (Jacob Sparre Andersen)
--    Added procedure Find_File.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Strings.Unbounded;

------------------------------------------------------------------------------
--  Other packages:

with String_Arrays;

------------------------------------------------------------------------------

package File_System is

   ---------------------------------------------------------------------------
   --  function Exists:

   function Exists (File_Name : in String) return Boolean;

   ---------------------------------------------------------------------------
   --  procedure Copy:
   --
   --  Copies a file named From to a file named To.

   procedure Copy (From : in     String;
                   To   : in     String);

   ---------------------------------------------------------------------------
   --  procedure Delete:
   --
   --  Deletes the file named Name.

   procedure Delete (Name : in     String);

   ---------------------------------------------------------------------------
   --  function Size:
   --
   --  Returns the file size in bytes.

   function Size (Name : in     String) return Natural;

   ---------------------------------------------------------------------------
   --  procedure Find_File:
   --
   --  Locates a file with the name File_Name in one of the catalogs in Path.
   --  If the file is found, File_Name is set to the full path and file name
   --  and Found_It is set to true.
   --  Otherwise Found_It is set to false, and File_Name is unchanged.

   procedure Find_File
     (File_Name : in out Ada.Strings.Unbounded.Unbounded_String;
      Path      : in     String_Arrays.String_Array_Reference;
      Found_It  :    out Boolean);

   ---------------------------------------------------------------------------

end File_System;
