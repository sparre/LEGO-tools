with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

------------------------------------------------------------------------------
--  Other packages:

with Generic_Command_Line_Processing;
with Generic_Command_Line_Types;
with Generic_Rectangular_Vectors;
with PGM;

------------------------------------------------------------------------------

procedure PGM_To_LDraw is

   type LEGO_Units is new Float;

   package LU_Vectors is
     new Generic_Rectangular_Vectors (Scalar     => LEGO_Units,
                                      Dimensions => 3);

   package Plain_Vectors is
     new Generic_Rectangular_Vectors (Scalar     => Float,
                                      Dimensions => 3);

   ---------------------------------------------------------------------------
   --  Private subprograms:

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     LEGO_Units);

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     LU_Vectors.Point);

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Plain_Vectors.Vector);

   procedure Put_Piece (File          : in     Ada.Text_IO.File_Type;
                        Colour_Number : in     Natural;
                        Position      : in     LU_Vectors.Point;
                        X, Y, Z       : in     Plain_Vectors.Vector;
                        Piece         : in     String);

   procedure Put_Column (File          : in     Ada.Text_IO.File_Type;
                         X, Y          : in     Integer;
                         Bottom, Top   : in     Integer;
                         Colour_Number : in     Natural);

   procedure Put_Tall_Column (File          : in     Ada.Text_IO.File_Type;
                              X, Y          : in     Integer;
                              Bottom, Top   : in     Integer;
                              Colour_Number : in     Natural);

   function Lowest_Neighbour
     (Landscape : in     PGM.Pixmaps_16_Bit.Pixmap_Reference;
      X, Y      : in     Natural) return Natural;

   procedure Load_Colour_Map
     (Item :    out PGM.Pixmaps_16_Bit.Pixmap_Reference);

   procedure Rescale (Landscape  : in out PGM.Pixmaps_16_Bit.Pixmap_Type;
                      New_Height : in     Positive);

   ---------------------------------------------------------------------------
   --  Command line processing:

   type Argument_Names is (Colour, Map, Minimal, Height, No_Edge);

   package Command_Line_Types is
     new Generic_Command_Line_Types (Argument_Names => Argument_Names);

   function U (Item : in     String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   package Command_Line_Processing is new Generic_Command_Line_Processing
     (Command_Line_Types  => Command_Line_Types,
      Obligatory          => (others => False),
      Minimum_Field_Count => (Colour | Map | Height => 1,
                              Minimal | No_Edge     => 0),
      Maximum_Field_Count => (Colour | Map | Height => 1,
                              Minimal | No_Edge     => 0),
      Help                => (Colour  => U ("<LDraw colour number>"),
                              Map     => U ("<Map file name>"),
                              Minimal => U (""),
                              Height  => U ("<In plates>"),
                              No_Edge => U ("")));

   ---------------------------------------------------------------------------

   procedure Load_Colour_Map
     (Item :    out PGM.Pixmaps_16_Bit.Pixmap_Reference) is
   begin
      PGM.Load (Name => Command_Line_Processing.Value (Argument => Map,
                                                       Index    => 1),
                Item => Item);
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "No colour map. Aborting ...");
         raise;
   end Load_Colour_Map;

   Ground_Edge : constant Boolean :=
                   not Command_Line_Processing.Set (No_Edge);

   function Lowest_Neighbour
     (Landscape : in     PGM.Pixmaps_16_Bit.Pixmap_Reference;
      X, Y      : in     Natural) return Natural is

      use PGM;

      Bottom : Grey_16_Bit := Landscape (X, Y);

   begin --  Lowest_Neighbour
      if Ground_Edge and (X = Landscape'First (1) or
                          X = Landscape'Last (1) or
                          Y = Landscape'First (2) or
                          Y = Landscape'Last (2))
      then
         Bottom := Grey_16_Bit'First;
      else
         for X_Index in Natural'Max (X - 1, Landscape'First (1)) ..
                        Natural'Min (X + 1, Landscape'Last (1)) loop
            for Y_Index in Natural'Max (Y - 1, Landscape'First (2)) ..
                           Natural'Min (Y + 1, Landscape'Last (2)) loop
               Bottom := Grey_16_Bit'Min (Bottom,
                                          Landscape (X_Index, Y_Index));
            end loop;
         end loop;
      end if;

      return Natural (Bottom);
   end Lowest_Neighbour;

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     LEGO_Units) is

      use Ada.Float_Text_IO;

   begin --  Put
      Put (File => File,
           Item => Float (Item * 4.0),
           Fore => 5,
           Aft  => 1);
   end Put;

   ---------------------------------------------------------------------------
   --  procedure Put:

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     LU_Vectors.Point) is

      use Ada.Text_IO;

   begin --  Put
      Put (File => File,
           Item => -Item (2));
      Put (File => File,
           Item => " ");
      Put (File => File,
           Item => -Item (3));
      Put (File => File,
           Item => " ");
      Put (File => File,
           Item => Item (1));
   end Put;

   ---------------------------------------------------------------------------
   --  procedure Put:

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Plain_Vectors.Vector) is

      use Ada.Float_Text_IO;
      use Ada.Text_IO;

   begin --  Put
      Put (File => File,
           Item => Item (1),
           Fore => 2,
           Aft  => 3);
      Put (File => File,
           Item => " ");
      Put (File => File,
           Item => Item (2),
           Fore => 2,
           Aft  => 3);
      Put (File => File,
           Item => " ");
      Put (File => File,
           Item => Item (3),
           Fore => 2,
           Aft  => 3);
   end Put;

   procedure Put_Column (File          : in     Ada.Text_IO.File_Type;
                         X, Y          : in     Integer;
                         Bottom, Top   : in     Integer;
                         Colour_Number : in     Natural) is

      Level : Integer := Bottom;

   begin --  Put_Column
      while Level <= Top loop
         if Level mod 3 = 0 and then Level + 2 <= Top then
            Put_Piece (File          => File,
                       Colour_Number => Colour_Number,
                       Position      => (1 => LEGO_Units (5 * X),
                                         2 => LEGO_Units (5 * Y),
                                         3 => LEGO_Units (2 * Level) + 4.0),
                       X             => (1.0, 0.0, 0.0),
                       Y             => (0.0, 1.0, 0.0),
                       Z             => (0.0, 0.0, 1.0),
                       Piece         => "3005.dat");
            Level := Level + 3;
         else
            Put_Piece (File          => File,
                       Colour_Number => Colour_Number,
                       Position      => (1 => LEGO_Units (5 * X),
                                         2 => LEGO_Units (5 * Y),
                                         3 => LEGO_Units (2 * Level)),
                       X             => (1.0, 0.0, 0.0),
                       Y             => (0.0, 1.0, 0.0),
                       Z             => (0.0, 0.0, 1.0),
                       Piece         => "3024.dat");
            Level := Level + 1;
         end if;
      end loop;
   end Put_Column;

   ---------------------------------------------------------------------------
   --  procedure Put_Piece:

   procedure Put_Piece (File          : in     Ada.Text_IO.File_Type;
                        Colour_Number : in     Natural;
                        Position      : in     LU_Vectors.Point;
                        X, Y, Z       : in     Plain_Vectors.Vector;
                        Piece         : in     String) is

      use Ada.Integer_Text_IO;
      use Ada.Text_IO;

   begin --  Put_Piece
      Put (File => File,
           Item => " 1  ");

      Put (File  => File,
           Item  => Colour_Number,
           Width => 3);
      Put (File => File,
           Item => "  ");

      Put (File => File,
           Item => Position);
      Put (File => File,
           Item => "  ");

      Put (File => File,
           Item => X);
      Put (File => File,
           Item => "  ");

      Put (File => File,
           Item => Y);
      Put (File => File,
           Item => "  ");

      Put (File => File,
           Item => Z);
      Put (File => File,
           Item => "  ");

      Put_Line (File => File,
                Item => Piece);
   end Put_Piece;

   ---------------------------------------------------------------------------
   --  procedure Put_Column:
   --
   --  Writes a column of 1x1x1 and 1x1x1/3 bricks.

   ---------------------------------------------------------------------------
   --  procedure Put_Tall_Column:
   --
   --  Writes a column of 1x1x1 and 1x1x1/3 bricks.

   procedure Put_Tall_Column (File          : in     Ada.Text_IO.File_Type;
                              X, Y          : in     Integer;
                              Bottom, Top   : in     Integer;
                              Colour_Number : in     Natural) is

   begin --  Put_Tall_Column
      for Level in Bottom .. Top loop
         if (Level - Top) mod 15 = 0 then
            Put_Piece (File          => File,
                       Colour_Number => Colour_Number,
                       Position      => (1 => LEGO_Units (5 * X),
                                         2 => LEGO_Units (5 * Y),
                                         3 => LEGO_Units (2 * Level)),
                       X             => (1.0, 0.0, 0.0),
                       Y             => (0.0, 1.0, 0.0),
                       Z             => (0.0, 0.0, 1.0),
                       Piece         => "2453.dat");
         end if;
      end loop;
   end Put_Tall_Column;

   procedure Rescale (Landscape  : in out PGM.Pixmaps_16_Bit.Pixmap_Type;
                      New_Height : in     Positive) is
      Bottom : PGM.Grey_16_Bit := PGM.Grey_16_Bit'Last;
      Top    : PGM.Grey_16_Bit := PGM.Grey_16_Bit'First;
   begin
      for X in Landscape'Range (1) loop
         for Y in Landscape'Range (2) loop
            Bottom := PGM.Grey_16_Bit'Min (Bottom, Landscape (X, Y));
            Top    := PGM.Grey_16_Bit'Max (Top,    Landscape (X, Y));
         end loop;
      end loop;

      declare
         use type PGM.Grey_16_Bit;

         Old_Height : constant Positive := Integer (Top - Bottom);
      begin
         for X in Landscape'Range (1) loop
            for Y in Landscape'Range (2) loop
               Landscape (X, Y) :=
                 PGM.Grey_16_Bit (Integer (Landscape (X, Y) - Bottom)
                                    * New_Height / Old_Height);
            end loop;
         end loop;
      end;
   end Rescale;

   ---------------------------------------------------------------------------

   use Ada.Text_IO;
   use PGM;
   use PGM.Pixmaps_16_Bit;

   Landscape, Colour_Map : Pixmap_Reference;
   Colour_Number         : Natural := 16;
   Use_Tall_Bricks       : constant Boolean := Command_Line_Processing.Set
                                                 (Minimal);

begin
   Ada.Float_Text_IO.Default_Exp := 0;

   Load (File => Current_Input,
         Item => Landscape);

   if Command_Line_Processing.Set (Height) then
      Rescale (Landscape  => Landscape.all,
               New_Height => Command_Line_Processing.Value (Height, 1));
   end if;

   if Command_Line_Processing.Set (Colour) then
      Colour_Number := Command_Line_Processing.Value (Colour, 1);

      for X in Landscape'Range (1) loop
         for Y in Landscape'Range (2) loop
            if Use_Tall_Bricks then
               Put_Tall_Column
                 (File          => Current_Output,
                  X             => X - Landscape'Length (1) / 2,
                  Y             => Y - Landscape'Length (2) / 2,
                  Bottom        => Lowest_Neighbour (Landscape, X, Y),
                  Top           => Natural (Landscape (X, Y)),
                  Colour_Number => Colour_Number);
            else
               Put_Column
                 (File          => Current_Output,
                  X             => X - Landscape'Length (1) / 2,
                  Y             => Y - Landscape'Length (2) / 2,
                  Bottom        => Lowest_Neighbour (Landscape, X, Y),
                  Top           => Natural (Landscape (X, Y)),
                  Colour_Number => Colour_Number);
            end if;
         end loop;
      end loop;

      Put_Line (File => Current_Output,
                Item => "0 STEP");
   elsif Command_Line_Processing.Set (Map) then
      Load_Colour_Map (Item => Colour_Map);

      for X in Landscape'Range (1) loop
         for Y in Landscape'Range (2) loop
            if Use_Tall_Bricks then
               Put_Tall_Column
                 (File          => Current_Output,
                  X             => X - Landscape'Length (1) / 2,
                  Y             => Y - Landscape'Length (2) / 2,
                  Bottom        => Lowest_Neighbour (Landscape, X, Y),
                  Top           => Natural (Landscape (X, Y)),
                  Colour_Number => Natural (Colour_Map (X, Y)));
            else
               Put_Column
                 (File          => Current_Output,
                  X             => X - Landscape'Length (1) / 2,
                  Y             => Y - Landscape'Length (2) / 2,
                  Bottom        => Lowest_Neighbour (Landscape, X, Y),
                  Top           => Natural (Landscape (X, Y)),
                  Colour_Number => Natural (Colour_Map (X, Y)));
            end if;
         end loop;
      end loop;

      Put_Line (File => Current_Output,
                Item => "0 STEP");
   else
      Put_Line ("You should either give a '-colour' or a '-map' argument.");
      raise Command_Line_Processing.Argument_Error;
   end if;
exception
   when others =>
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Current_Error,
         Item => "PGM_To_LDraw: An undocumented exception was raised. " &
                 "Aborting ...");
         raise;
end PGM_To_LDraw;
