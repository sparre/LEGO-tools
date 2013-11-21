------------------------------------------------------------------------------
--
--  package Random_Numbers (spec)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.04.27 (Jacob Sparre Andersen)
--    Written.
--
--  1998.04.05 (Jacob Sparre Andersen)
--    Renamed procedure Init_Gauss to Reset.
--    Added function Even.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

package Random_Numbers is

   ---------------------------------------------------------------------------
   --  procedure Reset:

   procedure Reset;

   ---------------------------------------------------------------------------
   --  function Even:

   function Even return Float;

   ---------------------------------------------------------------------------
   --  function Gauss:

   function Gauss return Float;

   ---------------------------------------------------------------------------  

end Random_Numbers;
