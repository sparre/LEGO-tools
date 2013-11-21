------------------------------------------------------------------------------
--
--  package Random_Numbers (body)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.04.27 (Jacob Sparre Andersen)
--    Written based on the Borland Pascal unit Fractal_Images.
--
--  1998.04.05 (Jacob Sparre Andersen)
--    Renamed procedure Init_Gauss to Reset.
--    Added function Even.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;

package body Random_Numbers is

   ---------------------------------------------------------------------------
   --  Constants:

   Nrand : constant Natural := 4;
   Arand : constant Float := 1.0;

   GaussAdd : constant Float :=
                Ada.Numerics.Elementary_Functions.Sqrt (Float (3 * Nrand));
   GaussFac : constant Float := 2.0 * GaussAdd / (Float (Nrand) * Arand);

   ---------------------------------------------------------------------------
   --  Random number generator:

   Even_Distribution  : Ada.Numerics.Float_Random.Generator;

   ---------------------------------------------------------------------------
   --  procedure Reset:

   procedure Reset is

      use Ada.Numerics.Float_Random;

   begin --  Reset
      Reset (Even_Distribution);
   end Reset;

   ---------------------------------------------------------------------------
   --  function Even:

   function Even return Float is

      use Ada.Numerics.Float_Random;

   begin --  Even
      return Random (Even_Distribution);
   end Even;

   ---------------------------------------------------------------------------
   --  function Gauss:

   function Gauss return Float is

      use Ada.Numerics.Float_Random;

      Sum : Float := 0.0;

   begin --  Gauss
      for i in 1 .. Nrand loop
         Sum := Sum + Random (Even_Distribution);
      end loop;

      return GaussFac * Sum - GaussAdd;
   end Gauss;

   --------------------------------------------------------------------------- 

end Random_Numbers;
