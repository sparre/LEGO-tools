with
  Ada.Text_IO;
with
  PPM;

procedure Outline_Boundaries is
   use type PPM.Pixel;

   Drawing       : constant PPM.Pixmap_24_Bit :=
                     PPM.Load (Ada.Text_IO.Standard_Input);
   With_Outlines : PPM.Pixmap_24_Bit (Drawing'Range (1), Drawing'Range (2));
begin
   for X in Drawing'Range (1) loop
      for Y in Drawing'Range (2) loop
         if X = Drawing'First (1) or X = Drawing'Last (1) then
            With_Outlines (X, Y) := (others => 0);
         elsif Y = Drawing'First (2) or Y = Drawing'Last (2) then
            With_Outlines (X, Y) := (others => 0);
         elsif Drawing (X, Y) /= Drawing (X, Y - 1) then
            With_Outlines (X, Y) := (others => 0);
         elsif Drawing (X, Y) /= Drawing (X, Y + 1) then
            With_Outlines (X, Y) := (others => 0);
         elsif Drawing (X, Y) /= Drawing (X - 1, Y) then
            With_Outlines (X, Y) := (others => 0);
         elsif Drawing (X, Y) /= Drawing (X + 1, Y) then
            With_Outlines (X, Y) := (others => 0);
         else
            With_Outlines (X, Y) := Drawing (X, Y);
         end if;
      end loop;
   end loop;

   PPM.Save (File => Ada.Text_IO.Standard_Output,
             Item => With_Outlines);
end Outline_Boundaries;
