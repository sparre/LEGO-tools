------------------------------------------------------------------------------
--
--  package File_System (body)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1996.05.17 (Jacob Sparre Andersen)
--    Written.
--
--  1996.10.31 (Jacob Sparre Andersen)
--    Using package Debugged. Modified error reporting.
--
--  1996.11.21 (Jacob Sparre Andersen)
--    Fixed a bug in procedure Exists.
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
--  1998.04.15 (Jacob Sparre Andersen)
--    Dropped debugging messages.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO;
with Ada.Text_IO;

package body File_System is

   ---------------------------------------------------------------------------
   --  Exists:

   function Exists (File_Name : in String) return Boolean is

      use Ada.Text_IO;

      Dummy : File_Type;

   begin --  Exists
      Open  (File => Dummy, Name => File_Name, Mode => In_File);
      Close (File => Dummy);

      return True;
   exception
      when Name_Error =>
         return False;
      when others =>
         Put_Line
           (File => Ada.Text_IO.Current_Output,
            Item => "File_System.Exists: Unexpected exception.");
         raise;
   end Exists;

   ---------------------------------------------------------------------------
   --  procedure Copy:
   --
   --  Copies a file named From to a file named To.

   procedure Copy (From : in     String;
                   To   : in     String) is

      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      Buffer         : Stream_Element_Array (0 .. 4095);
      Filled_To      : Stream_Element_Offset;
      Source, Target : File_Type;

   begin --  Copy
      Open (File => Source,
            Name => From,
            Mode => In_File);
      Create (File => Target,
              Name => To,
              Mode => Out_File);

      while not End_Of_File (File => Source) loop
         Read (File => Source,
               Item => Buffer,
               Last => Filled_To);
         Write (File => Target,
                Item => Buffer (Buffer'First .. Filled_To));
      end loop;

      Close (File => Source);
      Close (File => Target);
   end Copy;

   ---------------------------------------------------------------------------
   --  procedure Delete:
   --
   --  Deletes the file named Name.

   procedure Delete (Name : in     String) is

      use Ada.Streams.Stream_IO;

      File : File_Type;

   begin --  Delete
      Open (File => File,
            Name => Name,
            Mode => In_File);
      Delete (File => File);
   end Delete;

   ---------------------------------------------------------------------------
   --  function Size:
   --
   --  Returns the file size in bytes.

   function Size (Name : in     String) return Natural is

      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      File   : File_Type;
      Result : Natural;

   begin --  Size
      Open (File => File,
            Name => Name,
            Mode => In_File);
      Result := Natural (Size (File => File)) * Stream_Element'Size / 8;
      Close (File => File);

      return Result;
   end Size;

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
      Found_It  :    out Boolean) is

      use Ada.Strings.Unbounded;

   begin --  Find_File
      for Index in Path'Range loop
         if Exists (To_String (Path (Index) & File_Name)) then
            File_Name := Path (Index) & File_Name;
            Found_It := True;
            return;
         elsif Exists (To_String (Path (Index) & "/" & File_Name)) then
            File_Name := Path (Index) & "/" & File_Name;
            Found_It := True;
            return;
         end if;
      end loop;

      Found_It := False;
   end Find_File;

   ---------------------------------------------------------------------------

end File_System;