------------------------------------------------------------------------------
--
--  package PGM (body)
--
--  This package is used to read PGM (portable graymap file format) files.
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.03.26 (Jacob Sparre Andersen)
--    Written.
--
--  1997.07.08 (Jacob Sparre Andersen)
--    Modified procedure Save, so it enhances the pixmap to use the full
--      intensity scale.
--
--  1997.10.24 (Jacob Sparre Andersen)
--    Added access type versions of the Load and Save procedures.
--
--  2001.07.30 (Jacob Sparre Andersen)
--    Save now creates files in stead of only opening existing ones.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;

------------------------------------------------------------------------------

package body PGM is

   ---------------------------------------------------------------------------
   --  Magic number:

   PGM_Magic_Number : constant String := "P2";

   ---------------------------------------------------------------------------
   --  package Grey_16_Bit_Text_IO:

   package Grey_16_Bit_Text_IO is new Ada.Text_IO.Modular_IO (Grey_16_Bit);

   ---------------------------------------------------------------------------
   --  procedure Skip_Comments (private):

   procedure Skip_Comments (File : in     Ada.Text_IO.File_Type) is

      package Latin_1 renames Ada.Characters.Latin_1;

      use Ada.Text_IO;

      Next           : Character;
      At_End_Of_Line : Boolean;

   begin --  Skip_Comments
      loop
         Look_Ahead (File        => File,
                     Item        => Next,
                     End_Of_Line => At_End_Of_Line);

         if At_End_Of_Line then
            Skip_Line (File => File);
         else
            case Next is
               when '#' =>
                  Skip_Line (File => File);
               when Latin_1.Space | Latin_1.HT | Latin_1.LF | Latin_1.CR =>
                  Get (File => File,
                       Item => Next);
               when others =>
                  exit;
            end case;
         end if;
      end loop;
   end Skip_Comments;

   ---------------------------------------------------------------------------
   --  procedure Get (private):

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Positive) is

   begin --  Get
      Skip_Comments (File => File);
      Ada.Integer_Text_IO.Get (File => File,
                               Item => Item);
   end Get;
   ---------------------------------------------------------------------------
   --  procedure Get (private):

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Grey_16_Bit) is

   begin --  Get
      Skip_Comments (File => File);
      Grey_16_Bit_Text_IO.Get (File => File,
                               Item => Item);
   end Get;

   ---------------------------------------------------------------------------
   --  procedure Load:
   --
   --  Reads a PGM file.

   procedure Load (File : in     Ada.Text_IO.File_Type;
                   Item :    out Pixmap_16_Bit) is

      Magic_Number       : String (1 .. 2);
      Width, Height      : Positive;
      Maximum_Grey_Value : Grey_16_Bit;
      Pixel              : Grey_16_Bit;

   begin --  Load
      Ada.Text_IO.Get (File => File,
                       Item => Magic_Number);

      if Magic_Number = PGM_Magic_Number then
         Get (File => File,
              Item => Width);
         Get (File => File,
              Item => Height);
         Get (File => File,
              Item => Maximum_Grey_Value);

         for Y in reverse 1 .. Height loop
            for X in 1 .. Width loop
               Get (File => File,
                    Item => Pixel);

               if X <= Item'Length (1) and Y <= Item'Length (2) then
                  Item (X + Item'First (1) - 1,
                        Y + Item'First (2) - 1) := Pixel;
               end if;
            end loop;
         end loop;
      else
         raise Invalid_File_Format;
      end if;
   end Load;

   ---------------------------------------------------------------------------
   --  procedure Load:
   --
   --  Reads a PGM file.

   procedure Load (File : in     Ada.Text_IO.File_Type;
                   Item :    out Pixmaps_16_Bit.Pixmap_Reference) is

      Magic_Number       : String (1 .. 2);
      Width, Height      : Positive;
      Maximum_Grey_Value : Grey_16_Bit;
      Pixel              : Grey_16_Bit;

   begin --  Load
      Ada.Text_IO.Get (File => File,
                       Item => Magic_Number);

      if Magic_Number = PGM_Magic_Number then
         Get (File => File,
              Item => Width);
         Get (File => File,
              Item => Height);
         Get (File => File,
              Item => Maximum_Grey_Value);

         Item := new Pixmaps_16_Bit.Pixmap_Type (1 .. Width, 1 .. Height);

         for Y in reverse 1 .. Height loop
            for X in 1 .. Width loop
               Get (File => File,
                    Item => Pixel);

               if X <= Item'Length (1) and Y <= Item'Length (2) then
                  Item (X + Item'First (1) - 1,
                        Y + Item'First (2) - 1) := Pixel;
               end if;
            end loop;
         end loop;
      else
         raise Invalid_File_Format;
      end if;
   end Load;

   ---------------------------------------------------------------------------
   --  procedure Load:
   --
   --  Reads a PGM file.

   procedure Load (Name : in     String;
                   Item :    out Pixmap_16_Bit) is

      use Ada.Text_IO;

      File : File_Type;

   begin --  Load
      Open (File => File,
            Name => Name,
            Mode => Ada.Text_IO.In_File);
      Load (File => File,
            Item => Item);
      Close (File => File);
   end Load;

   ---------------------------------------------------------------------------
   --  procedure Load:
   --
   --  Reads a PGM file.

   procedure Load (Name : in     String;
                   Item :    out Pixmaps_16_Bit.Pixmap_Reference) is

      use Ada.Text_IO;

      File : File_Type;

   begin --  Load
      Open (File => File,
            Name => Name,
            Mode => Ada.Text_IO.In_File);
      Load (File => File,
            Item => Item);
      Close (File => File);
   end Load;

   ---------------------------------------------------------------------------
   --  function Load:
   --
   --  Reads a PGM file.

   function Load (File : in     Ada.Text_IO.File_Type) return Pixmap_16_Bit is

      Magic_Number       : String (1 .. 2);
      Width, Height      : Positive;
      Maximum_Grey_Value : Grey_16_Bit;

   begin --  Load
      Ada.Text_IO.Get (File => File,
                       Item => Magic_Number);

      if Magic_Number = PGM_Magic_Number then
         Get (File => File,
              Item => Width);
         Get (File => File,
              Item => Height);
         Get (File => File,
              Item => Maximum_Grey_Value);

      Read_Pixels:
         declare

            Result : Pixmap_16_Bit (1 .. Width, 1 .. Height);

         begin --  Read_Pixels
            for Y in reverse 1 .. Height loop
               for X in 1 .. Width loop
                  Get (File => File,
                       Item => Result (X, Y));
               end loop;
            end loop;

            return Result;
         end Read_Pixels;
      else
         raise Invalid_File_Format;
      end if;
   end Load;

   ---------------------------------------------------------------------------
   --  function Load:
   --
   --  Reads a PGM file.

   function Load (Name : in     String) return Pixmap_16_Bit is

      use Ada.Text_IO;

      File : File_Type;

   begin --  Load
      Open (File => File,
            Name => Name,
            Mode => Ada.Text_IO.In_File);

      return Load (File => File);
   end Load;

   ---------------------------------------------------------------------------
   --  procedure Save:
   --
   --  Writes a PGM file.

   procedure Save (File : in     Ada.Text_IO.File_Type;
                   Item : in     Pixmap_16_Bit) is

      use Ada.Integer_Text_IO;
      use Ada.Text_IO;
      use Grey_16_Bit_Text_IO;

      Minimum_Grey_Value : Grey_16_Bit := Grey_16_Bit'Last;
      Maximum_Grey_Value : Grey_16_Bit := Grey_16_Bit'First;

   begin --  Save
      for X in Item'Range (1) loop
         for Y in Item'Range (2) loop
            Minimum_Grey_Value := Grey_16_Bit'Min (Minimum_Grey_Value,
                                                   Item (X, Y));
            Maximum_Grey_Value := Grey_16_Bit'Max (Maximum_Grey_Value,
                                                   Item (X, Y));
         end loop;
      end loop;

      Put_Line (File => File,
                Item => PGM_Magic_Number);

      Put (File => File,
           Item => Integer (Item'Length (1)));
      Put (File => File,
           Item => " ");
      Put (File => File,
           Item => Integer (Item'Length (2)));
      New_Line (File => File);

      Put (File => File,
           Item => Maximum_Grey_Value - Minimum_Grey_Value);
      New_Line (File => File);

      for Y in reverse Item'Range (2) loop
         for X in Item'Range (1) loop
            Put (File => File,
                 Item => Item (X, Y) - Minimum_Grey_Value);

            if Col (File) > 60 then
               New_Line (File => File);
            else
               Put (File => File,
                    Item => " ");
            end if;
         end loop;
      end loop;
   end Save;

   ---------------------------------------------------------------------------
   --  procedure Save:
   --
   --  Writes a PGM file.

   procedure Save (File : in     Ada.Text_IO.File_Type;
                   Item : in     Pixmaps_16_Bit.Pixmap_Reference) is

      use Ada.Integer_Text_IO;
      use Ada.Text_IO;
      use Grey_16_Bit_Text_IO;

      Minimum_Grey_Value : Grey_16_Bit := Grey_16_Bit'Last;
      Maximum_Grey_Value : Grey_16_Bit := Grey_16_Bit'First;

   begin --  Save
      for X in Item'Range (1) loop
         for Y in Item'Range (2) loop
            Minimum_Grey_Value := Grey_16_Bit'Min (Minimum_Grey_Value,
                                                   Item (X, Y));
            Maximum_Grey_Value := Grey_16_Bit'Max (Maximum_Grey_Value,
                                                   Item (X, Y));
         end loop;
      end loop;

      Put_Line (File => File,
                Item => PGM_Magic_Number);

      Put (File => File,
           Item => Integer (Item'Length (1)));
      Put (File => File,
           Item => " ");
      Put (File => File,
           Item => Integer (Item'Length (2)));
      New_Line (File => File);

      Put (File => File,
           Item => Maximum_Grey_Value - Minimum_Grey_Value);
      New_Line (File => File);

      for Y in reverse Item'Range (2) loop
         for X in Item'Range (1) loop
            Put (File => File,
                 Item => Item (X, Y) - Minimum_Grey_Value);

            if Col (File) > 60 then
               New_Line (File => File);
            else
               Put (File => File,
                    Item => " ");
            end if;
         end loop;
      end loop;
   end Save;

   ---------------------------------------------------------------------------
   --  procedure Save:
   --
   --  Writes a PGM file.

   procedure Save (Name : in     String;
                   Item : in     Pixmap_16_Bit) is

      use Ada.Text_IO;

      File : File_Type;

   begin --  Save
      Create (File => File,
              Name => Name,
              Mode => Out_File);
      Save (File => File,
            Item => Item);
      Close (File => File);
   end Save;

   ---------------------------------------------------------------------------
   --  procedure Save:
   --
   --  Writes a PGM file.

   procedure Save (Name : in     String;
                   Item : in     Pixmaps_16_Bit.Pixmap_Reference) is

      use Ada.Text_IO;

      File : File_Type;

   begin --  Save
      Create (File => File,
              Name => Name,
              Mode => Out_File);
      Save (File => File,
            Item => Item);
      Close (File => File);
   end Save;

   ---------------------------------------------------------------------------

end PGM;
