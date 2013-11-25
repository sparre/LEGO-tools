with
  Ada.Streams.Stream_IO;

package body File_System is

   procedure Copy (From : in     String;
                   To   : in     String) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      Buffer         : Stream_Element_Array (0 .. 4095);
      Filled_To      : Stream_Element_Offset;
      Source, Target : File_Type;
   begin
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

   procedure Delete (Name : in     String) is
      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Open (File => File,
            Name => Name,
            Mode => In_File);
      Delete (File => File);
   end Delete;

   procedure Find_File
     (File_Name : in out Ada.Strings.Unbounded.Unbounded_String;
      Path      : in     String_Arrays.Instance;
      Found_It  :    out Boolean) is

      use Ada.Directories, Ada.Strings.Unbounded;
   begin
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

   function Size (Name : in     String) return Natural is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      File   : File_Type;
      Result : Natural;
   begin
      Open (File => File,
            Name => Name,
            Mode => In_File);
      Result := Natural (Size (File => File)) * Stream_Element'Size / 8;
      Close (File => File);

      return Result;
   end Size;

end File_System;
