------------------------------------------------------------------------------
--
--  procedure Fractal_Landscape (body)
--
--  This procedure generates a fractal landscape based on the parameters 
--  'H' and 'Z_Max'.
--
--  The size of the landscape is set with the parameters 'width' and 'height'.
--
------------------------------------------------------------------------------
--  Update information:
--
--  1997.04.29 (Jacob Sparre Andersen)
--    Written.
--
--  1997.04.30 (Jacob Sparre Andersen)
--    Added a 'z_max' command line argument.
--   
--  1997.05.09 (Jacob Sparre Andersen)
--    Removed the '-Sigma' argument.
--    Writing the fractal dimension to Current_Error.
--
--  2001.08.10 (Jacob Sparre Andersen)
--    Made the variable Landscape an access type.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

------------------------------------------------------------------------------
--  Other packages:

with Fractal_Images;
with Generic_Command_Line_Processing;
with Generic_Command_Line_Types;
with PGM;

------------------------------------------------------------------------------

procedure Fractal_Landscape is

   ---------------------------------------------------------------------------
   --  type Argument_Names:

   type Argument_Names is (Width, Height, H, Addition, Z_Max);

   ---------------------------------------------------------------------------
   --  package Command_Line_Types:

   package Command_Line_Types is
     new Generic_Command_Line_Types (Argument_Names => Argument_Names);

   ---------------------------------------------------------------------------
   --  function U:

   function U (Item : in     String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   ---------------------------------------------------------------------------
   --  package Command_Line_Processing:

   package Command_Line_Processing is new Generic_Command_Line_Processing
     (Command_Line_Types  => Command_Line_Types,
      Obligatory          => (Width | Height | H => True,
                              Addition | Z_Max   => False),
      Minimum_Field_Count => (Width | Height | H | Z_Max => 1,
                              Addition                   => 0),
      Maximum_Field_Count => (Width | Height | H | Z_Max => 1,
                              Addition                   => 0),
      Help                => (Width | Height => U ("<size in pixels>"),
                              H              => U ("<float> - in the range " &
                                                     "]0;1["),
                              Addition       => U (" - more spiky landscape"),
                              Z_Max          => U ("<integer> - height of " &
                                                     "the landscape in " &
                                                     "plates")));

   ---------------------------------------------------------------------------
   --  function Subset:

   function Subset (Grid         : in     Fractal_Images.Grid_Reference;
                    Min_X, Max_X : in     Integer;
                    Min_Y, Max_Y : in     Integer;
                    Min_Z        : in     PGM.Grey_16_Bit :=
                                            PGM.Grey_16_Bit'First;
                    Max_Z        : in     PGM.Grey_16_Bit :=
                                            PGM.Grey_16_Bit'Last)
     return PGM.Pixmaps_16_Bit.Pixmap_Reference is

      use PGM;
      use PGM.Pixmaps_16_Bit;

      Min_Float_Z : Float := Float'Last;
      Max_Float_Z : Float := Float'First;
      Pixmap      : Pixmap_Reference;

      Offset, Factor : Float;

   begin --  Subset
      Pixmap := new Pixmap_Type (Min_X .. Max_X, Min_Y .. Max_Y);

      if Min_X in Grid'Range (1) and Max_X in Grid'Range (1) and
         Min_Y in Grid'Range (2) and Max_Y in Grid'Range (2) then

         for X in Min_X .. Max_X loop
            for Y in Min_Y .. Max_Y loop
               Min_Float_Z := Float'Min (Min_Float_Z, Grid (X, Y));
               Max_Float_Z := Float'Max (Min_Float_Z, Grid (X, Y));
            end loop;
         end loop;

         Offset := - Min_Float_Z;
         Factor := Float (Max_Z - Min_Z) / (Max_Float_Z - Min_Float_Z);

         for X in Min_X .. Max_X loop
            for Y in Min_Y .. Max_Y loop
               Pixmap (X, Y) :=
                 Min_Z + Grey_16_Bit ((Grid (X, Y) + Offset) * Factor);
            end loop;
         end loop;

         return Pixmap;
      else
         raise Constraint_Error;
      end if;
   exception
      when Constraint_Error =>
         raise;
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Fractal_Landscape.Subset: An undocumented exception " &
                    "was raised. Propagating it ...");
         raise;
   end Subset;

   ---------------------------------------------------------------------------

   Minimum_Width  : Positive renames Command_Line_Processing.Value (Width, 1);
   Minimum_Height : Positive renames Command_Line_Processing.Value (Height, 1);

   Size : Positive;

begin --  Fractal_Landscape
   for Power in 0 .. 255 loop
      Size := 2 ** Power + 1;

      exit when (Minimum_Width <= Size) and (Minimum_Height <= Size);
   end loop;

   declare

      use Ada.Float_Text_IO;
      use Ada.Text_IO;
      use Fractal_Images;
      use PGM;

      Landscape : Grid_Reference;
      Max_Z     : Grey_16_Bit := Grey_16_Bit'Last;
      H_Arg     : Float := Command_Line_Processing.Value (H, 1);

   begin
      Landscape := new Grid_Type (1 .. Size, 1 .. Size);

      if Command_Line_Processing.Set (Z_Max) then
         Max_Z :=
           Grey_16_Bit'Value (Command_Line_Processing.Value (Z_Max, 1));
      end if;

      if 0.0 < H_Arg and H_Arg < 1.0 then
         Put (File => Current_Error,
              Item => "Creating a fractal landscape with dimension ");
         Put (File => Current_Error,
              Item => 3.0 - H_Arg);
         Put (File => Current_Error,
              Item => " ...");
         New_Line (File => Current_Error);

         Mid_Point_FM_2D (Grid     => Landscape.all,
                          Sigma    => 1.0,
                          H        => H_Arg,
                          Addition => Command_Line_Processing.Set (Addition));

         Save (File => Current_Output,
               Item => Subset (Grid  => Landscape,
                               Min_X => 1,
                               Max_X => Minimum_Width,
                               Min_Y => 1,
                               Max_Y => Minimum_Height,
                               Min_Z => 0,
                               Max_Z => Max_Z));
      else
         Put_Line
           (File => Current_Error,
            Item => "H (the number after '-h' on the command line) should " &
                    "be in the range ]0;1[. This results in a fractal " &
                    "dimension D = 3 - H");
      end if;
   end;
exception
   when others =>
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Current_Error,
         Item => "Fractal_Landscape: An undocumented exception was raised. " &
                 "Aborting ...");
         raise;
end Fractal_Landscape;
