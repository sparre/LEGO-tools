------------------------------------------------------------------------------
--
--  package PGM (spec)
--
--  This package is used to read PGM (portable graymap file format) files.
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.03.26 (Jacob Sparre Andersen)
--    Written.
--
--  1997.10.24 (Jacob Sparre Andersen)
--    Added access type versions of the Load and Save procedures.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Text_IO;

------------------------------------------------------------------------------
--  Other packages:

with Pixmaps;

------------------------------------------------------------------------------

package PGM is

   ---------------------------------------------------------------------------
   --  Exceptions:

   Invalid_File_Format : exception;

   ---------------------------------------------------------------------------
   --  type Grey_16_Bit:

   type Grey_16_Bit is mod 2 ** 16;

   ---------------------------------------------------------------------------
   --  package Pixmaps_16_Bit:

   package Pixmaps_16_Bit is new Pixmaps (Grey_16_Bit);

   ---------------------------------------------------------------------------
   --  subtype Pixmap_16_Bit:

   subtype Pixmap_16_Bit is Pixmaps_16_Bit.Pixmap_Type;

   ---------------------------------------------------------------------------
   --  procedure Load:
   --
   --  Reads a PGM file.

   procedure Load (File : in     Ada.Text_IO.File_Type;
                   Item :    out Pixmap_16_Bit);

   ---------------------------------------------------------------------------
   --  procedure Load:
   --
   --  Reads a PGM file.

   procedure Load (File : in     Ada.Text_IO.File_Type;
                   Item :    out Pixmaps_16_Bit.Pixmap_Reference);

   ---------------------------------------------------------------------------
   --  procedure Load:
   --
   --  Reads a PGM file.

   procedure Load (Name : in     String;
                   Item :    out Pixmap_16_Bit);

   ---------------------------------------------------------------------------
   --  procedure Load:
   --
   --  Reads a PGM file.

   procedure Load (Name : in     String;
                   Item :    out Pixmaps_16_Bit.Pixmap_Reference);

   ---------------------------------------------------------------------------
   --  function Load:
   --
   --  Reads a PGM file.

   function Load (File : in     Ada.Text_IO.File_Type) return Pixmap_16_Bit;

   ---------------------------------------------------------------------------
   --  function Load:
   --
   --  Reads a PGM file.

   function Load (Name : in     String) return Pixmap_16_Bit;

   ---------------------------------------------------------------------------
   --  procedure Save:
   --
   --  Writes a PGM file.

   procedure Save (File : in     Ada.Text_IO.File_Type;
                   Item : in     Pixmap_16_Bit);

   ---------------------------------------------------------------------------
   --  procedure Save:
   --
   --  Writes a PGM file.

   procedure Save (File : in     Ada.Text_IO.File_Type;
                   Item : in     Pixmaps_16_Bit.Pixmap_Reference);

   ---------------------------------------------------------------------------
   --  procedure Save:
   --
   --  Writes a PGM file.

   procedure Save (Name : in     String;
                   Item : in     Pixmap_16_Bit);

   ---------------------------------------------------------------------------
   --  procedure Save:
   --
   --  Writes a PGM file.

   procedure Save (Name : in     String;
                   Item : in     Pixmaps_16_Bit.Pixmap_Reference);

   ---------------------------------------------------------------------------

end PGM;
