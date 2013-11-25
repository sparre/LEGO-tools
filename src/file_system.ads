with
  Ada.Directories,
  Ada.Strings.Unbounded;
with
  String_Arrays;

package File_System is

   function Exists (File_Name : in String) return Boolean
     renames Ada.Directories.Exists;
   pragma Obsolescent (Exists, "Please use Ada.Directories.Exists.");

   procedure Copy (From : in     String;
                   To   : in     String);
   --  Copies a file named From to a file named To.

   procedure Delete (Name : in     String);
   --  Deletes the file named Name.

   function Size (Name : in     String) return Natural;
   --  Returns the file size in bytes.

   procedure Find_File
     (File_Name : in out Ada.Strings.Unbounded.Unbounded_String;
      Path      : in     String_Arrays.Instance;
      Found_It  :    out Boolean);
   --  Locates a file with the name File_Name in one of the catalogs in Path.
   --  If the file is found, File_Name is set to the full path and file name
   --  and Found_It is set to True.
   --  Otherwise Found_It is set to False, and File_Name is unchanged.

end File_System;
