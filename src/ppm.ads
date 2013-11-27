with
  Ada.Text_IO;
with
  Pixmaps;

package PPM is

   Invalid_File_Format : exception;

   type Intensity is range 0 .. 255
     with Size => 8;
   type Primary_Colours is (Red, Green, Blue);
   type Pixel is array (Primary_Colours) of Intensity
     with Size => 24;

   package Pixmaps_24_Bit is new Pixmaps (Pixel);
   subtype Pixmap_24_Bit is Pixmaps_24_Bit.Pixmap_Type;

   function Load (File : in     Ada.Text_IO.File_Type) return Pixmap_24_Bit;

   function Load (Name : in     String) return Pixmap_24_Bit;

   procedure Save (File : in     Ada.Text_IO.File_Type;
                   Item : in     Pixmap_24_Bit);

   procedure Save (Name : in     String;
                   Item : in     Pixmap_24_Bit);

end PPM;
