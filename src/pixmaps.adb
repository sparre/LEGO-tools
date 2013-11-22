package body Pixmaps is

   procedure Mirror_X (Pixmap : in out Pixmap_Type) is

   begin
      for Y in Pixmap'Range (2) loop
         declare
            Column : constant Pixmap_Type :=
                       Subset (Pixmap => Pixmap,
                               Min_X  => Pixmap'First (1),
                               Max_X  => Pixmap'Last (1),
                               Min_Y  => Y,
                               Max_Y  => Y);
         begin
            for X in Pixmap'Range (1) loop
               Pixmap (Pixmap'Last (1) + Pixmap'First (1) - X, Y) :=
                 Column (X, Y);
            end loop;
         end;
      end loop;
   end Mirror_X;

   procedure Mirror_Y (Pixmap : in out Pixmap_Type) is

   begin
      for X in Pixmap'Range (1) loop
         declare
            Row : constant Pixmap_Type :=
                    Subset (Pixmap, X, X, Pixmap'First (2), Pixmap'Last (2));
         begin
            for Y in Pixmap'Range (2) loop
               Pixmap (X, Pixmap'Last (2) + Pixmap'First (2) - Y) :=
                 Row (X, Y);
            end loop;
         end;
      end loop;
   end Mirror_Y;

   function Rotate_180_deg (Pixmap : in Pixmap_Type) return Pixmap_Type is

      Result : Pixmap_Type (-Pixmap'Last (1) .. -Pixmap'First (1),
                            -Pixmap'Last (2) .. -Pixmap'First (2));

   begin
      for x in Pixmap'Range (1) loop
         for y in Pixmap'Range (2) loop
            Result (-x, -y) := Pixmap (x, y);
         end loop;
      end loop;
      return Result;
   end Rotate_180_deg;

   function Rotate_270_deg (Pixmap : in Pixmap_Type) return Pixmap_Type is

      Result : Pixmap_Type (Pixmap'Range (2),
                           -Pixmap'Last (1) .. -Pixmap'First (1));

   begin
      for x in Pixmap'Range (1) loop
         for y in Pixmap'Range (2) loop
            Result (y, -x) := Pixmap (x, y);
         end loop;
      end loop;
      return Result;
   end Rotate_270_deg;

   function Rotate_90_deg (Pixmap : in Pixmap_Type) return Pixmap_Type is

      Result : Pixmap_Type (-Pixmap'Last (2) .. -Pixmap'First (2),
                           Pixmap'Range (1));

   begin
      for x in Pixmap'Range (1) loop
         for y in Pixmap'Range (2) loop
            Result (-y, x) := Pixmap (x, y);
         end loop;
      end loop;
      return Result;
   end Rotate_90_deg;

   function Subset (Pixmap       : in     Pixmap_Type;
                    Min_X, Max_X : in     Integer;
                    Min_Y, Max_Y : in     Integer) return Pixmap_Type is

      Result : Pixmap_Type (Min_X .. Max_X,
                            Min_Y .. Max_Y);

   begin --  Subset
      for X in Min_X .. Max_X loop
         for Y in Min_Y .. Max_Y loop
            Result (X, Y) := Pixmap (X, Y);
         end loop;
      end loop;

      return Result;
   end Subset;

end Pixmaps;
