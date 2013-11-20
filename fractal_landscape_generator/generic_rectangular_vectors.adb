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

with Ada.Strings.Unbounded;                     
with Ada.Numerics.Generic_Elementary_Functions;

package body Generic_Rectangular_Vectors is

   ---------------------------------------------------------------------------
   --  package Elementary_Functions:

   package Elementary_Functions is new 
     Ada.Numerics.Generic_Elementary_Functions(Float_Type => Scalar);
   
   ---------------------------------------------------------------------------
   --  Operations with Vector result

   function "+" (Left, Right : in Vector) return Vector is
      Result : Vector;
   begin
      for Index in Left'Range loop
         Result(Index) := Left(Index) + Right(Index);
      end loop;
      return Result;
   end "+";

   function "-"( Left , Right : in Vector ) return Vector is
      Result : Vector;
   begin
      for Index in Result'Range loop
         Result(Index) := Left(Index) - Right(Index);
      end loop;
      return Result;
   end "-";

   function "-"( Left , Right : in Point ) return Vector is
      Result : Vector;
   begin
      for Index in Result'Range loop
         Result(Index) := Left(Index) - Right(Index);
      end loop;
      return Result;
   end "-";

   function "-"(Right : in Vector) return Vector is
      Result : Vector;
   begin
      for Index in Result'Range loop
         Result(Index) := -Right(Index);
      end loop;
      return Result;
   end "-";

   function "*"(Left  : in Vector;
		Right : in Scalar) return Vector is
      Result : Vector;
   begin
      for Index in Result'Range loop
         Result(Index) := Left(Index) * Right;
      end loop;
      return Result;
   end "*";

   function "*"(Left  : in Scalar;
		Right : in Vector) return Vector is
      Result : Vector;
   begin
      for Index in Result'Range loop
         Result(Index) := Left * Right(Index);
      end loop;
      return Result;
   end "*";

   function "/"(Left  : in Vector;
		Right : in Scalar) return Vector is
      Result : Vector;
   begin
      for Index in Result'Range loop
         Result(Index) := Left(Index) / Right;
      end loop;
      return Result;
   end "/";

   ---------------------------------------------------------------------------
   -- Operations with Point result:

   function "+" (Left  : in Point;
                 Right : in Vector) return Point is
      Result : Point;
   begin
      for Index in Result'Range loop
         Result(Index) := Left(Index) + Right(Index);
      end loop;
      return Result;
   end "+";

   function "-" (Left  : in Point;
                 Right : in Vector) return Point is
      Result : Point;
   begin
      for Index in Left'Range loop
         Result(Index) := Left(Index) - Right(Index);
      end loop;
      return Result;
   end "-";

   ---------------------------------------------------------------------------
   --  Operations with scalar result:

   function "*"(Left, Right : in Vector) return Scalar is
      Scalar_product : Scalar := 0.0;
   begin
      for Index in Left'Range loop
         Scalar_product := Scalar_product + Left(Index) * Right(Index);
      end loop;
      return Scalar_product;
   end "*";

   ---------------------------------------------------------------------------
   --  Various subroutines:

   function Length (Item : in Vector) return Scalar is

   begin --  Length
      return Elementary_Functions.Sqrt ( Squared_Length (Item));
   end Length;

   function Squared_Length (Item : in Vector) return Scalar is

      Result : Scalar := 0.0;

   begin --  Squared_Length
      for Index in Vector'Range loop
         Result := Result + Item (Index) ** 2;
      end loop;

      return Result;
   end Squared_Length;

   procedure Normalize (Item : in out Vector) is

   begin --  Normalize
      Item := Item / Length (Item);
   end Normalize;

   function Unit_Vector (Item : in Vector) return Vector is

   begin --  Unit_Vector
      return Item / Length (Item);
   end Unit_Vector;

   function Project (V  : in Vector;
                     On : in Vector) return Vector is

   begin --  Project
      return On * (V * On) / Squared_Length (On);
   end Project;

   function Mirror (Ray            : in Vector;
                    Surface_Normal : in Vector) return Vector is

   begin --  Mirror
      return Ray - 2.0 * Project (Ray, Surface_Normal);
   end Mirror;

   function Distance (A, B : in Point) return Scalar is

      pragma Inline (Distance);

   begin --  Distance
      return Length (A - B);
   end Distance;

   function Squared_Distance (A, B : in Point) return Scalar is

      pragma Inline (Squared_Distance);

   begin --  Squared_Distance
      return Squared_Length (A - B);
   end Squared_Distance;


   ---------------------------------------------------------------------------
   --  Image and Value:

   function Image (Item : in Vector) return String is

      use Ada.Strings.Unbounded;

      Result : Unbounded_String;

   begin
      Result := To_Unbounded_String ("(");

      for Index in Item'Range loop
         Result := Result & Scalar'Image ( Item (Index));
         
         if Index = Item'Last then
            Result := Result & ")";
         else
            Result := Result & ",";
         end if;
      end loop;

      return To_String (Result);
   end Image;

   function Image (Item : in Point) return String is

      use Ada.Strings.Unbounded;

      Result : Unbounded_String;

   begin
      Result := To_Unbounded_String ("(");

      for Index in Item'Range loop
         Result := Result & Scalar'Image ( Item (Index));
         
         if Index = Item'Last then
            Result := Result & ")";
         else
            Result := Result & ",";
         end if;
      end loop;

      return To_String (Result);
   end Image;

   function Value (Item : in String) return Vector is

      Result : Vector := Null_Vector;

   begin --  Value
      return Result;
   end Value;

   function Value (Item : in String) return Point is

      Result : Point := Origo;

   begin --  Value
      return Result;
   end Value;

   ---------------------------------------------------------------------------

end Generic_Rectangular_Vectors;
