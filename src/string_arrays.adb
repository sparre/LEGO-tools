with
  Ada.Strings.Fixed;

package body String_Arrays is

   ---------------------------------------------------------------------------
   --  procedure Append:
   --
   --  Splits the string Source into an array of words and appends it to
   --  Target:
   --
   --  Exceptions:
   --    Constraint_Error - if Word_Separators is empty.

   procedure Append (Source          : in     String;
                     Word_Separators : in     Ada.Strings.Maps.Character_Set;
                     Target          : in out String_Array_Reference) is

      use Ada.Strings.Unbounded;

   begin --  Append
      Append (Source          => To_Unbounded_String (Source),
              Word_Separators => Word_Separators,
              Target          => Target);
   end Append;

   procedure Append
     (Source          : in     Ada.Strings.Unbounded.Unbounded_String;
      Word_Separators : in     Ada.Strings.Maps.Character_Set;
      Target          : in out String_Array_Reference) is

      New_Words : String_Array_Reference;

   begin --  Append
      Split (Source          => Source,
             Word_Separators => Word_Separators,
             Target          => New_Words);

      if Target = null then
         Target := New_Words;
      else
         Target := new String_Array_Type'(Target.all & New_Words.all);
      end if;
   end Append;

   ---------------------------------------------------------------------------
   --  procedure Append:
   --
   --  Appends the string Source to Target:
   --
   --  Exceptions:
   --    (none)

   procedure Append (Source : in     String;
                     Target : in out String_Array_Reference) is

      use Ada.Strings.Unbounded;

   begin --  Append
      Append (Source => To_Unbounded_String (Source),
              Target => Target);
   end Append;

   procedure Append (Source : in     Ada.Strings.Unbounded.Unbounded_String;
                     Target : in out String_Array_Reference) is

   begin --  Append
      if Target = null then
         Target := new String_Array_Type'((1 => Source));
      else
         Target := new String_Array_Type'(Target.all & (1 => Source));
      end if;
   end Append;

   ---------------------------------------------------------------------------
   --  function Index:
   --
   --  Returns the index of the first occurring instance of Pattern in the
   --  list. The result is 0 (zero) if Pattern does not occur in the list.
   --
   --  Exceptions: (none)

   function Index (Source  : in     String_Array_Type;
                   Pattern : in     String) return Natural is
   begin
      for Result in Source'Range loop
         if Ada.Strings.Unbounded.To_String (Source (Result)) = Pattern then
            return Result;
         end if;
      end loop;

      return 0;
   end Index;

   function Index (Source  : in     String_Array_Type;
                   Pattern : in     Ada.Strings.Unbounded.Unbounded_String)
     return Natural is

      use Ada.Strings.Unbounded;

   begin --  Index
      for Result in Source'Range loop
         if Source (Result) = Pattern then
            return Result;
         end if;
      end loop;

      return 0;
   end Index;

   ---------------------------------------------------------------------------
   --  function Is_Subset:
   --
   --  Checks if all of Elements can be found in Set.

   function Is_Subset (Elements : in     String_Array_Type;
                       Set      : in     String_Array_Type) return Boolean is

      use Ada.Strings.Unbounded;

      Counted : array (Set'Range) of Boolean := (others => False);
      Found   : array (Elements'Range) of Boolean := (others => False);

   begin --  Is_Subset
      for Element in Elements'Range loop
         for Index in Set'Range loop
            if Counted (Index) then
               null;
            elsif Elements (Element) = Set (Index) then
               Counted (Index) := True;
               Found (Element) := True;
            end if;
         end loop;

         if Found (Element) then
            null;
         else
            return False;
         end if;
      end loop;

      return True;
   end Is_Subset;

   ---------------------------------------------------------------------------
   --  procedure Remove_Subset:
   --
   --  Removes all elements in Elements from Set.
   --
   --  Exception:
   --    Constraint_Error - if Elements is not a subset of Set.

   procedure Remove_Subset (Elements      : in     String_Array_Type;
                            Set           : in out String_Array_Reference;
                            All_instances : in     Boolean := False) is

      use Ada.Strings.Unbounded;

      Remove : array (Set.all'Range)  of Boolean := (others => False);
      Found  : array (Elements'Range) of Boolean := (others => False);

      New_Set : constant String_Array_Reference :=
                  new String_Array_Type (1 .. Set'Length - Elements'Length);
      Counter : Natural := 0;

   begin --  Remove_Subset

      Identify_Elements :
      for Element in Elements'Range loop

         Scan_Set :
         for Index in Set.all'Range loop
            if Remove (Index) then
               null;
            elsif Elements (Element) = Set.all (Index) then
               Remove (Index) := True;
               Found (Element) := True;

               if All_instances then
                  null; --  Continue!
               else
                  exit Scan_Set;
               end if;
            end if;
         end loop Scan_Set;

         if Found (Element) then
            null;
         else
            raise Constraint_Error;
         end if;
      end loop Identify_Elements;

      Copy_Elements :
      for Index in Set.all'Range loop
         if Remove (Index) then
            null;
         else
            Counter := Counter + 1;
            New_Set (Counter) := Set (Index);
         end if;
      end loop Copy_Elements;

      Set := New_Set;
   end Remove_Subset;

   ---------------------------------------------------------------------------
   --  procedure Split:
   --
   --  Splits a string into an array of words:
   --
   --  Exceptions:
   --    Constraint_Error - if Word_Separators is empty.

   procedure Split
     (Source                 : in     String;
      Word_Separators        : in     Ada.Strings.Maps.Character_Set;
      Target                 :    out String_Array_Reference;
      Skip_Repeat_Separators : in     Boolean := True) is

      use Ada.Strings.Unbounded;

   begin --  Split
      Split (Source                 => To_Unbounded_String (Source),
             Word_Separators        => Word_Separators,
             Target                 => Target,
             Skip_Repeat_Separators => Skip_Repeat_Separators);
   end Split;

   procedure Split
     (Source                 : in     Ada.Strings.Unbounded.Unbounded_String;
      Word_Separators        : in     Ada.Strings.Maps.Character_Set;
      Target                 :    out String_Array_Reference;
      Skip_Repeat_Separators : in     Boolean := True) is

      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      use Ada.Strings.Unbounded;

      Separator_Sequence : Character_Sequence renames
                             To_Sequence (Word_Separators);
      Separator          : String renames Separator_Sequence (1 .. 1);

      Raw    : Unbounded_String := Source & Separator;

   begin --  Split
      Translate (Source  => Raw,
                 Mapping => To_Mapping (From => Separator_Sequence,
                                        To   => Separator_Sequence'Length
                                                  * Separator));

      Trim (Source => Raw,
            Left   => Word_Separators,
            Right  => Null_Set);

      if Skip_Repeat_Separators then
         while Index (Source  => Raw,
                      Pattern => Separator & Separator) > 0 loop
            Delete (Source  => Raw,
                    From    => Index (Source  => Raw,
                                      Pattern => Separator & Separator),
                    Through => Index (Source  => Raw,
                                      Pattern => Separator & Separator));
         end loop;
      end if;

      Target := new String_Array_Type (1 .. Count (Source  => Raw,
                                                   Pattern => Separator));

      for Word in Target.all'Range loop
         Target.all (Word) :=
           To_Unbounded_String (Slice (Source => Raw,
                                       Low    => 1,
                                       High   => Index (Source  => Raw,
                                                        Pattern => Separator)
                                                   - 1));
         Delete (Source  => Raw,
                 From    => 1,
                 Through => Index (Source  => Raw,
                                   Pattern => Separator));
      end loop;
   end Split;

   ---------------------------------------------------------------------------
   --  function Split:
   --
   --  Splits a string into an array of words:
   --
   --  Exceptions:
   --    Constraint_Error - if Word_Separators is empty.

   function Split
     (Source                 : in     String;
      Word_Separators        : in     Ada.Strings.Maps.Character_Set;
      Skip_Repeat_Separators : in     Boolean := True)
     return String_Array_Type is

      Result : String_Array_Reference;

   begin --  Split
      Split (Source                 => Source,
             Word_Separators        => Word_Separators,
             Target                 => Result,
             Skip_Repeat_Separators => Skip_Repeat_Separators);
      return Result.all;
   end Split;

   ---------------------------------------------------------------------------

end String_Arrays;
