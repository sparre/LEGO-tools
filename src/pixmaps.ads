------------------------------------------------------------------------------
--
--  package Pixmaps (spec)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1995.11.21 (Jacob Sparre Andersen)
--    Written.
--
--  1996.04.05 (Jacob Sparre Andersen)
--    Format changed to fit the "Style Guide-look".
--
--  1997.09.29 (Jacob Sparre Andersen)
--    Added type Pixmap_Reference.
--
-- (Add update information above this line.)
------------------------------------------------------------------------------

generic

   type Pixel_Type is private;

package Pixmaps is

   ---------------------------------------------------------------------------
   --  type Pixmap_Type:
   --
   --  The pixmap is considered to be mapped to a right-hand coordinate
   --  system, -- e.g. x going from left to right and y going from bottom to
   --  top.

   type Pixmap_Type is array (Integer range <>, Integer range <>)
     of Pixel_Type;

   type Pixmap_Reference is access all Pixmap_Type;

   function Subset (Pixmap       : in Pixmap_Type;
                    Min_X, Max_X : in Integer;
                    Min_Y, Max_Y : in Integer) return Pixmap_Type;

   function Rotate_90_deg (Pixmap : in Pixmap_Type) return Pixmap_Type;
   --  Rotating the pixmap 90° around origo.

   function Rotate_180_deg (Pixmap : in Pixmap_Type) return Pixmap_Type;
   --  Rotating the pixmap 180° around origo.

   function Rotate_270_deg (Pixmap : in Pixmap_Type) return Pixmap_Type;
   --  Rotating the pixmap 270° around origo.

   procedure Mirror_X (Pixmap : in out Pixmap_Type);
   --  Mirror around the x-axis.

   procedure Mirror_Y (Pixmap : in out Pixmap_Type);
   --  Mirror around the y-axis.

end Pixmaps;
