with
  Ada.Characters.Latin_1,
  Ada.Integer_Text_IO;

package body PPM is

   PPM_Magic_Number : constant String := "P3";

   package Intensity_Text_IO is new Ada.Text_IO.Integer_IO (Intensity);

   procedure Skip_Comments (File : in     Ada.Text_IO.File_Type);

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Positive);

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Intensity);

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Pixel);

   ---------------------------------------------------------------------------

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Positive) is
   begin
      Skip_Comments (File => File);
      Ada.Integer_Text_IO.Get (File => File,
                               Item => Item);
   end Get;

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Intensity) is
   begin
      Skip_Comments (File => File);
      Intensity_Text_IO.Get (File => File,
                             Item => Item);
   end Get;

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Pixel) is
   begin
      for Component of Item loop
         Get (File => File,
              Item => Component);
      end loop;
   end Get;

   ---------------------------------------------------------------------------

   function Load (File : in     Ada.Text_IO.File_Type) return Pixmap_24_Bit is
      Magic_Number      : String (1 .. 2);
      Width, Height     : Positive;
      Maximum_Intensity : Intensity;
   begin
      Ada.Text_IO.Get (File => File,
                       Item => Magic_Number);

      if Magic_Number = PPM_Magic_Number then
         Get (File => File,
              Item => Width);
         Get (File => File,
              Item => Height);
         Get (File => File,
              Item => Maximum_Intensity);

         if Maximum_Intensity = Intensity'Last then
            return Result : Pixmap_24_Bit (1 .. Width, 1 .. Height)
            do
               for Y in reverse Result'Range (2) loop
                  for X in Result'Range (1) loop
                     Get (File => File,
                          Item => Result (X, Y));
                  end loop;
               end loop;
            end return;
         else
            raise Invalid_File_Format;
         end if;
      else
         raise Invalid_File_Format;
      end if;
   end Load;

   function Load (Name : in     String) return Pixmap_24_Bit is
      use Ada.Text_IO;

      Source : File_Type;
   begin
      Open  (File => Source,
             Name => Name,
             Mode => In_File);
      return Load  (File => Source);
   end Load;

   procedure Save (File : in     Ada.Text_IO.File_Type;
                   Item : in     Pixmap_24_Bit) is
      use Ada.Integer_Text_IO;
      use Ada.Text_IO;
      use Intensity_Text_IO;
   begin
      Put_Line (File => File,
                Item => PPM_Magic_Number);

      Put (File  => File,
           Item  => Integer'(Item'Length (1)),
           Width => 0);
      Put (File  => File,
           Item  => " ");
      Put (File  => File,
           Item  => Integer'(Item'Length (2)),
           Width => 0);
      New_Line (File => File);

      Put (File  => File,
           Item  => Intensity'Last,
           Width => 0);
      New_Line (File => File);

      for Y in reverse Item'Range (2) loop
         for X in Item'Range (1) loop
            for Component of Item (X, Y) loop
               Put (File  => File,
                    Item  => Component,
                    Width => 3);
               Put (File  => File,
                    Item  => " ");
            end loop;
         end loop;

         New_Line (File => File);
      end loop;
   end Save;

   procedure Save (Name : in     String;
                   Item : in     Pixmap_24_Bit) is
      use Ada.Text_IO;

      File : File_Type;
   begin
      Create (File => File,
              Name => Name,
              Mode => Out_File);
      Save   (File => File,
              Item => Item);
      Close  (File => File);
   end Save;

   procedure Skip_Comments (File : in     Ada.Text_IO.File_Type) is
      package Latin_1 renames Ada.Characters.Latin_1;

      use Ada.Text_IO;

      Next           : Character;
      At_End_Of_Line : Boolean;
   begin
      loop
         Look_Ahead (File        => File,
                     Item        => Next,
                     End_Of_Line => At_End_Of_Line);

         if At_End_Of_Line then
            Skip_Line (File => File);
         else
            case Next is
               when '#' =>
                  Skip_Line (File => File);
               when Latin_1.Space | Latin_1.HT | Latin_1.LF | Latin_1.CR =>
                  Get (File => File,
                       Item => Next);
               when others =>
                  exit;
            end case;
         end if;
      end loop;
   end Skip_Comments;

end PPM;
