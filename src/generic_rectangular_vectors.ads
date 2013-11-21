------------------------------------------------------------------------------
--
--  package Generic_Rectangular_Vectors (spec)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1995.12.27 (Jacob Sparre Andersen)
--    Written.
--
--  1996.06.25 (Jacob Sparre Andersen)
--    Reformatted the header.
--    Corrected a few bugs.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

generic

   type Scalar is digits <>;

   Dimensions : Positive;

package Generic_Rectangular_Vectors is

   ---------------------------------------------------------------------------
   --  Types:

   type Vector is array (1 .. Dimensions) of Scalar;
   type Point  is new Vector;

   ---------------------------------------------------------------------------
   --  Constants:

   Null_Vector : constant Vector := (others => 0.0);
   Origo       : constant Point  := (others => 0.0);

   ---------------------------------------------------------------------------
   --  Operations with Vector result

   function "+" (Left, Right : in Vector) return Vector;

   function "-" (Left, Right : in Vector) return Vector;

   function "-" (Left, Right : in Point) return Vector;

   function "-" (Right : in Vector) return Vector;

   function "*" (Left  : in Scalar;
                 Right : in Vector) return Vector;

   function "*" (Left  : in Vector;
                 Right : in Scalar) return Vector;

   function "/" (Left  : in Vector;
                 Right : in Scalar) return Vector;

   ---------------------------------------------------------------------------
   -- Operations with Point result:

   function "+" (Left  : in Point;
                 Right : in Vector) return Point;

   function "-" (Left  : in Point;
                 Right : in Vector) return Point;

   ---------------------------------------------------------------------------
   --  Operations with scalar result:

   function "*" (Left, Right : in Vector) return Scalar;

   ---------------------------------------------------------------------------
   --  Various subroutines:

   function Length (Item : in Vector) return Scalar;

   function Squared_Length (Item : in Vector) return Scalar;

   procedure Normalize (Item : in out Vector);

   function Unit_Vector (Item : in Vector) return Vector;

   function Project (V  : in Vector;
                     On : in Vector) return Vector;

   function Mirror (Ray            : in Vector;
                    Surface_Normal : in Vector) return Vector;

   function Distance (A, B : in Point) return Scalar;

   function Squared_Distance (A, B : in Point) return Scalar;

   ---------------------------------------------------------------------------
   --  Image and Value:

   function Image (Item : in Vector) return String;

   function Image (Item : in Point) return String;

   function Value (Item : in String) return Vector;

   function Value (Item : in String) return Point;

   ---------------------------------------------------------------------------

end Generic_Rectangular_Vectors;
