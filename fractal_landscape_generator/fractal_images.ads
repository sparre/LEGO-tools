------------------------------------------------------------------------------
--
--  package Fractal_Images (spec)
--
------------------------------------------------------------------------------
--  Update information:
--
--  ?-1994.04.20 (Jacob Sparre Andersen)
--    Written.
--
--  1997.04.27-29 (Jacob Sparre Andersen)
--    Converted to Ada.
--
--  2001.08.10 (Jacob Sparre Andersen)
--    Added the access type Grid_Reference.
--
------------------------------------------------------------------------------

package Fractal_Images is

   ---------------------------------------------------------------------------
   --  type Grid_Type:

   type Grid_Type is array (Integer range <>, Integer range <>) of Float;

   type Grid_Reference is access all Grid_Type;

   ---------------------------------------------------------------------------
   --  procedure Mid_Point_FM_2D:
 
   procedure Mid_Point_FM_2D (Grid     :    out Grid_Type;
                              Sigma, H : in     Float;
                              Addition : in     Boolean);
 
   ---------------------------------------------------------------------------

end Fractal_Images;
