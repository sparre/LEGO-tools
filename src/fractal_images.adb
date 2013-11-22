with
  Ada.Numerics.Elementary_Functions,
  Ada.Text_IO;
with
  Random_Numbers;

package body Fractal_Images is

   ---------------------------------------------------------------------------
   --  procedure Mid_Point_FM_2D:

   procedure Mid_Point_FM_2D (Grid     :    out Grid_Type;
                              Sigma, H : in     Float;
                              Addition : in     Boolean) is
      use Random_Numbers;

      function f3 (Diff, x0, x1, x2 : in     Float) return Float is
         ((x0 + x1 + x2) / 3.0 + Diff * Gauss);

      function f4 (Diff, x0, x1, x2, x3 : in     Float) return Float is
         ((x0 + x1 + x2 + x3) / 4.0 + Diff * Gauss);

      function Ix (X : Natural) return Integer is
         (X + Grid'First (1));

      function Iy (Y : Natural) return Integer is
         (Y + Grid'First (2));

      procedure Inc (Item : in out Integer;
                     By   : in     Integer);
      procedure Inc (Item : in out Integer;
                     By   : in     Integer) is

      begin
         Item := Item + By;
      end Inc;

      use Ada.Numerics.Elementary_Functions;

      N      : Natural;
      Diff   : Float;
      x1     : Natural;
      y1     : Natural;
      d1, D2 : Natural;

      Max_Level : Natural := 0;

   begin --  Mid_Point_FM_2D
      --  Grid := (others => (others => 0.0));
      for Index_1 in Grid'Range (1) loop
         for Index_2 in Grid'Range (2) loop
            Grid (Index_1, Index_2) := 0.0;
         end loop;
      end loop;

      Find_Maximimum_Number_Of_Levels :
      loop
         exit Find_Maximimum_Number_Of_Levels
           when 2 ** (Max_Level + 1) + 1 > Grid'Length (1);
         exit Find_Maximimum_Number_Of_Levels
           when 2 ** (Max_Level + 1) + 1 > Grid'Length (2);

         Max_Level := Max_Level + 1;
      end loop Find_Maximimum_Number_Of_Levels;

      Reset;
      N := 2 ** Max_Level;
      Diff := Sigma;

      Grid (Ix (0), Iy (0)) := Diff * Gauss;
      Grid (Ix (0), Iy (N)) := Diff * Gauss;
      Grid (Ix (N), Iy (0)) := Diff * Gauss;
      Grid (Ix (N), Iy (N)) := Diff * Gauss;

      D2 := N;

      d1 := N / 2;

      for Stage in  1 .. Max_Level loop
         Diff := Diff * 0.5 ** (0.5 * H);

         x1 := d1;

         while x1 <= N - d1 loop
            y1 := d1;

            while y1 <= N - d1 loop
               Grid (Ix (x1), Iy (y1)) :=
                 f4 (Diff,
                     Grid (Ix (x1 + d1), Iy (y1 + d1)),
                     Grid (Ix (x1 + d1), Iy (y1 - d1)),
                     Grid (Ix (x1 - d1), Iy (y1 + d1)),
                     Grid (Ix (x1 - d1), Iy (y1 - d1)));
               Inc (y1, D2);
            end loop;

            Inc (x1, D2);
         end loop;

         if Addition then
            x1 := d1;

            while x1 <= N - d1 loop
               y1 := d1;

               while y1 <= N - d1 loop
                  Grid (Ix (x1), Iy (y1)) :=
                    Grid (Ix (x1), Iy (y1)) + Diff * Gauss;
                  Inc (y1, D2);
               end loop;

               Inc (x1, D2);
            end loop;
         end if;

         Diff := Diff * 0.5 ** (0.5 * H);

         x1 := d1;

         while x1 <= N - d1 loop
            Grid (Ix (x1), Iy (0)) := f3 (Diff,
                                          Grid (Ix (x1 + d1), Iy (0)),
                                          Grid (Ix (x1 - d1), Iy (0)),
                                          Grid (Ix (x1),      Iy (d1)));
            Grid (Ix (x1), Iy (N)) :=
              f3 (Diff,
                  Grid (Ix (x1 + d1), Iy (N)),
                  Grid (Ix (x1 - d1), Iy (N)),
                  Grid (Ix (x1),      Iy (N - d1)));
            Grid (Ix (0), Iy (x1)) :=
              f3 (Diff,
                  Grid (Ix (0),  Iy (x1 + d1)),
                  Grid (Ix (0),  Iy (x1 - d1)),
                  Grid (Ix (d1), Iy (x1)));
            Grid (Ix (N), Iy (x1)) :=
              f3 (Diff,
                  Grid (Ix (N),      Iy (x1 + d1)),
                  Grid (Ix (N),      Iy (x1 - d1)),
                  Grid (Ix (N - d1), Iy (x1)));
            Inc (x1, D2);
         end loop;

         x1 := d1;

         while x1 <= N - d1 loop
            y1 := D2;

            while y1 <= N - d1 loop
               Grid (Ix (x1), Iy (y1)) :=
                 f4 (Diff,
                     Grid (Ix (x1),      Iy (y1 + d1)),
                     Grid (Ix (x1),      Iy (y1 - d1)),
                     Grid (Ix (x1 + d1), Iy (y1)),
                     Grid (Ix (x1 - d1), Iy (y1)));

               Inc (y1, D2);
            end loop;

            Inc (x1, D2);
         end loop;

         x1 := D2;

         while x1 <= N - d1 loop
            y1 := d1;

            while y1 <= N - d1 loop
               Grid (Ix (x1), Iy (y1)) :=
                 f4 (Diff,
                     Grid (Ix (x1),      Iy (y1 + d1)),
                     Grid (Ix (x1),      Iy (y1 - d1)),
                     Grid (Ix (x1 + d1), Iy (y1)),
                     Grid (Ix (x1 - d1), Iy (y1)));

               Inc (y1, D2);
            end loop;

            Inc (x1, D2);
         end loop;

         if Addition then
            x1 := 0;

            while x1 <= N loop
               y1 := 0;

               while y1 <= N loop
                  Grid (Ix (x1), Iy (y1)) :=
                    Grid (Ix (x1), Iy (y1)) + Diff * Gauss;

                  Inc (y1, D2);
               end loop;

               Inc (x1, D2);
            end loop;

            x1 := d1;

            while x1 <= N - d1 loop
               y1 := d1;

               while y1 <= N - d1 loop
                  Grid (Ix (x1), Iy (y1)) :=
                    Grid (Ix (x1), Iy (y1)) + Diff * Gauss;
                  Inc (y1, D2);
               end loop;

               Inc (x1, D2);
            end loop;
         end if;

         D2 := D2 / 2;
         d1 := d1 / 2;
      end loop;
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Current_Error,
            Item => "Fractal_Images.Mid_Point_FM_2D: An undocumented " &
                    "exception was raised. Propagating it ...");
         raise;
   end Mid_Point_FM_2D;

   ---------------------------------------------------------------------------

end Fractal_Images;
