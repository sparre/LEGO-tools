--------------------------------- COPYRIGHT ------------------------------------
-- (C) 1993 Swiss Federal Institute of Technology (EPFL).                     --
--     Represented by A. Strohmeier EPFL-DI-LGL CH-1015 Lausanne Switzerland. --
--     All Rights Reserved.                                                   --
--------------------------------------------------------------------------------

--+ TITLE:    PACKAGE BODY FOR LISTS.
--+ REVISION: 16 October 1995 H. Ravaosolo, after code review
--+          Suppress the formal parameter Cirular in the function 
--+   Cursor_Position and add a new procedure Cursor_Position that has among 
--+   others things two out parameters Direction and Position. Change the name
--+   of the function Are_Twins by Are_Identical. Suppress the procedure
--+   Remove_All, it has the same behaviour as the procedure Destroy.
--+   Suppress the local procedure Choose_Moving_Dest_Index. Consequently if the
--+   destination index is Front in a copy or moving of a group of item then a 
--+   permutation of the start and end source index, and a changement of the
--+   direction is necessary to have the expected result.
--+ APPROVAL: 20-August-1995 N. Mouttet
--+ REVISION: 19-June-1995 H. Ravaosolo
--+           Suppress Empty_Structure_Error.
--+           Add comments to each local procedures and functions
--+           Use of "case" statement instead of "if" statement for test with 
--+ enumeration type to control later modification.
--+ CREATION: 10-Avril-1995 H.Ravaosolo.

with Text_IO;
with Unchecked_Deallocation;

package body List_Of_Static_Items_G is
-----------------------------------

  type Free_List_Type is
    record
      First_Free: Link_Type;
      Size: Natural := 0;
    end record;
  
  type Update_Type is (Down, Up);
  
  Max_Free_List_Size: Natural := 0;

  Free_List: Free_List_Type;                    -- list of free memory address


--/LOCAL PROCEDURES AND FUNCTIONS

  procedure Dispose is new Unchecked_Deallocation (Object => Cell_Type,
                                                   Name   => Link_Type);
                                                   
  pragma Inline (Dispose);
  
  
  procedure Create_And_Assign_Cell (Link: in out Link_Type;
                                    Item: in Item_Type) is
                                       
     pragma Inline (Create_And_Assign_Cell);
  
  --+ Overview: Creates an element which will be inserted to a list with the
  --+ initial value of Item. If the list of free memory is not empty then 
  --+ reuses an element from it.
  begin
    if Free_List.Size = 0 then
      Link := new Cell_Type;
    else
      Link := Free_List.First_Free;
      Free_List.First_Free := Free_List.First_Free.Next_Item_Link;
      Free_List.Size := Free_List.Size - 1;
    end if;
    Link.Item := Item;
  end Create_And_Assign_Cell;
  
  procedure Release (Link: in out Link_Type) is

     pragma Inline (Release);
 
  --+ Overview: Adds free memory to Free_List or returns it directly to the
  --+ system if Free_List is full.
  begin
    if Free_List.Size < Max_Free_List_Size then
      Link.Next_Item_Link := Free_List.First_Free;
      Free_List.First_Free := Link;
      Free_List.Size := Free_List.Size + 1;
    else
      Dispose (Link);
    end if;
  end Release;

  function Get_Cursor_Link (List: in List_Type;
                            This_Cursor: in Cursor_Type)
                            return Cursor_Link_Type is
  --+ Overview: Returns the state of This_Cursor. This function allows the 
  --+ placement of a temporary cursor at the position of a cursor's List or to 
  --+ save the state of the cursor.
  --+ The state of a cursor is given by the combination of its position and the
  --+ values of its two links, Previous and Next.
  begin
    case This_Cursor is 
      when Cursor => return List.Cursor;
      when Extra_Cursor => return List.Extra_Cursor;
    end case;
  end Get_Cursor_Link;
  

  procedure Init_Cursor_Position (List: in List_Type;
                                  At_Index: in Index_Type;
                                  Cursor_Link: in out Cursor_Link_Type) is
  --+ Overview: Places the temporary cursor Cursor_Link at the At_Index location
    This_Cursor: Cursor_Type;
  begin
    if Is_Empty (List) then
      Cursor_Link.Previous := null;
      Cursor_Link.Next := null;
      Cursor_Link.Position := 0;
    else
      case At_Index is
        when Front  => 
          Cursor_Link.Previous := null;
          Cursor_Link.Next := List.First_Item_Link;
          Cursor_Link.Position := 0;
        when Rear   => 
          Cursor_Link.Previous := List.Last_Item_Link;
          Cursor_Link.Next := null;
          Cursor_Link.Position := List.Size;
        when Cursor|Extra_Cursor => 
          This_Cursor := Cursor_Type(At_Index);
          Cursor_Link := Get_Cursor_Link (List, This_Cursor);
       end case;
    end if;
  end Init_Cursor_Position;
   

  procedure Put_Cursor_Link (List: in out List_Type;
                             This_Cursor: in Cursor_Type; 
                             Cursor_Link: in Cursor_Link_Type) is
  --+ Overview: Places the cursor This_Cursor of List at the location given by
  --+ Cursor_Link. This procedure changes the list.
  begin
    if This_Cursor = Cursor then
      List.Cursor := Cursor_Link;
    elsif This_Cursor = Extra_Cursor then
      List.Extra_Cursor := Cursor_Link;
    end if;
  end Put_Cursor_Link;
 
  
  procedure Move_Forward_Cursor (List: in List_Type;
                                 Cursor_Link: in out Cursor_Link_Type;
                                 Circular: in Boolean := False) is
  --+ Overview: Moves the temporary cursor Cursor_Link forward in List. If
  --+ Cursor_Link is at the end of the list and Circular is True, it will be
  --+ placed at the front of the list
  begin
    if Cursor_Link.Next = null then       
      case Circular is
        when True => 
          Cursor_Link.Previous := null;
          Cursor_Link.Next := List.First_Item_Link;
          Cursor_Link.Position := 0;
        when False => null;
      end case;
    else
      Cursor_Link.Previous := Cursor_Link.Next;
      Cursor_Link.Next := Cursor_Link.Next.Next_Item_Link;
      Cursor_Link.Position := Cursor_Link.Position + 1;
    end if;
  end Move_Forward_Cursor;
  
 
  procedure Move_Back_Cursor (List: in List_Type;
                              Cursor_Link: in out Cursor_Link_Type;
                              Circular: in Boolean := False) is
  --+ Overview: Moves back the temporary cursor Cursor_Link in List. If
  --+ Cursor_Link is at the front of the list and Circular is True, it will be
  --+ placed at the end of the list
  begin
    if Cursor_Link.Previous = null then        
      case Circular is
        when True => 
          Cursor_Link.Previous := List.Last_Item_Link;
          Cursor_Link.Next := null;
          Cursor_Link.Position := List.Size;
        when False => null;
      end case;
    else
      Cursor_Link.Next := Cursor_Link.Previous;           
      Cursor_Link.Previous := Cursor_Link.Previous.Previous_Item_Link;
      Cursor_Link.Position := Cursor_Link.Position - 1;
    end if;
  end Move_Back_Cursor;
  
  
  function Get_Link (List : in List_Type;
                     Index : in Index_Type;
                     Direction : in Direction_Type;
                     Circular : in Boolean) 
                     return Link_Type is
  --+ Overview: Returns the address given by the combination of Index and
  --+ Direction, and possibly Circular
    Cursor_Link: Cursor_Link_Type;
  begin
    case Index is 
      when Front =>
        case Direction is
          when Next => 
            return List.First_Item_Link;
          when Previous =>
            if Circular then
              return List.Last_Item_Link;
            else
              return null;
            end if;
        end case;
      when Rear => 
        case Direction is
          when Previous => 
            return List.Last_Item_Link;
          when Next =>
            if Circular then
              return List.First_Item_Link;
            else
              return null;
            end if;
        end case;
      when others => 
        Cursor_Link := Get_Cursor_Link (List, Cursor_Type (Index));
        case Direction is
          when Next => 
            if Cursor_Link.Next = null then
              case Circular is
                when true =>
                  return List.First_Item_Link;
                when False => return null;
              end case;
            else
              return Cursor_Link.Next;
            end if;
          when Previous =>
            if Cursor_Link.Previous = null then
              case Circular is
                when true =>
                  return List.Last_Item_Link;
                when False => return null;
              end case;
            else
              return Cursor_Link.Previous;
            end if;
        end case;
    end case;
  end Get_Link;
  
    
  function Get_Direction (List : in List_Type;
                          Index_S : in Index_Type;
                          Index_D : in Index_Type)
                          return Direction_Type is
  --+ Overview: Returns the direction result of Index_D position compared to
  --+ Index_S position
    Cursor_Link, Cursor_Link_D: Cursor_Link_Type;
  begin                                                 
    Init_Cursor_Position (List, Index_S, Cursor_Link);
    Init_Cursor_Position (List, Index_D, Cursor_Link_D);
    if Cursor_Link.Position < Cursor_Link_D.Position then
      return Next;
    else
      return Previous;
    end if;
  end Get_Direction;
  
  
  procedure Manage_Cursor (List : in out List_Type;
                           Index : in Index_Type := Front;
                           Update : in Update_Type := Up) is
  --+ Overview: Updates the state of the cursors's List after an insertion or a
  --+ remove. The cursor that corresponds to Index do not need to be updated. 
  --+ It was done in the function or procedure that calls Manage_Cursor.
    Temp_Direction: Direction_Type;
    Cursor_Link1, Cursor_Link2: Cursor_Link_Type;
  begin
    for Cursor_Loop in Cursor_Type loop
      Cursor_Link1 := Get_Cursor_Link (List, Cursor_Loop);
      case index is
        when Front =>
          if Cursor_Link1.Previous = null then
            Cursor_Link1.Next := List.First_Item_Link;
          else
            case Update is
              when Up => 
                Cursor_Link1.Position := Cursor_Link1.Position + 1;
              when Down =>
                if Cursor_Link1.Next = List.First_Item_Link then
                  Cursor_Link1.Previous := null;
                end if;
                Cursor_Link1.Position := Cursor_Link1.Position - 1;
            end case;
          end if;
        when Rear =>
          if Cursor_Link1.Next = null then
            case Update is
              when Up => 
                Cursor_Link1.Next := List.Last_Item_Link;
              when Down =>
                Cursor_Link1.Previous := List.Last_Item_Link;
                Cursor_Link1.Position := Cursor_Link1.Position - 1;
            end case;
          else
            case Update is
              when Up => 
                null;
              when Down =>
                if Cursor_Link1.Previous = List.Last_Item_Link then
                  Cursor_Link1.Next := null;
                end if;
            end case;
          end if;
        when Cursor|Extra_cursor =>    
          if Index /= Index_Type (Cursor_Loop) then    
            Cursor_Link2 := Get_Cursor_Link (List, Cursor_Type(Index));
            case Update is
              when Up =>
                if Cursor_Link1.Next = Cursor_Link2.Next then
                  Cursor_Link1.Next := Cursor_Link2.Previous;
                elsif Get_Direction (List,Index,Index_Type(Cursor_Loop))=Next
                  then Cursor_Link1.Position := Cursor_Link1.Position + 1;
                end if;
              when Down =>
                if (Cursor_Link1.Previous = Cursor_Link2.Previous) or
                                  (Cursor_Link1.Next = Cursor_Link2.Next) then
                  Cursor_Link1 := Cursor_Link2;
                elsif Get_Direction (List,Index,Index_Type(Cursor_Loop))=Next
                  then Cursor_Link1.Position := Cursor_Link1.Position - 1;
                end if;
            end case;
          end if;
      end case;
      Put_Cursor_Link (List, Cursor_Loop, Cursor_Link1);
    end loop;
  end Manage_Cursor;
  
  
  function Other_Direction (Direction: in Direction_Type)
                          return Direction_Type is
  --+ Overview: Returns the opposite value of Direction
  begin
    if Direction = Next then
      return Previous;
    else
      return Next;
    end if;
  end Other_Direction;


  function Is_Between (List: in List_Type;
                       Start_Index: in Index_Type;
                       End_Index: in Index_Type;
                       This_Index: in Index_Type;
                       Direction: in Direction_Type := Next;
                       Circular: in Boolean := False)
                       return Boolean is                     
  --+ Overview: Returns True if This_Index is located between Start_Index and
  --+ End_Index according to Direction. This fuction is useful in the case
  --+ that a copy or moving of a group of elements is done in a single list.
    Temp_Start: Cursor_Link_Type;
    Temp_End: Cursor_Link_Type;
    Temp_This: Cursor_Link_Type;
    Stop: Boolean := False;
  begin
    Init_Cursor_Position(List, Start_Index, Temp_Start);
    Init_Cursor_Position(List, End_Index, Temp_End);
    Init_Cursor_Position(List, This_Index, Temp_This);
    while not Stop loop
      case Direction is
        when Next => Move_Forward_Cursor (List, Temp_Start, Circular);
        when Previous =>Move_Back_Cursor (List, Temp_Start, Circular);
      end case;
      if Temp_Start = Temp_This or Temp_Start = Temp_End then
        Stop := True;
      end if;
    end loop;
    if Temp_Start = Temp_This then
      return True;
    else
      return False;
    end if;
  end Is_Between;
      
               
  procedure Create_Intermediate (List: in List_Type;
                                 List_Int: in out List_Type;
                                 Start_Index: in Index_Type;
                                 End_Index: in Index_Type;
                                 Direction: in Direction_Type := Next; 
                                 Circular: in Boolean := False) is    
  --+ Overview: Creates a temporary list. This procedure is useful in the case
  --+ that a copy or moving of a group of elements is done in a single list
  --+ and the destination index is located between the two source indexes.
    Temp_Start : Cursor_Link_Type;
    Temp_End : Cursor_Link_Type;
  begin
    Init_Cursor_Position (List, Start_Index, Temp_Start);
    Init_Cursor_Position (List, End_Index, Temp_End);
    case Direction is                                         
      when Next => 
        loop
          if Temp_Start.Next = null then
            Move_Forward_Cursor (List, Temp_Start, Circular);
          end if;
          Insert (List_Int, Temp_Start.Next.Item);
          Move_Forward_Cursor (List, Temp_Start, Circular);
          exit when Temp_Start = Temp_End;
        end loop;
      when Previous =>
        loop
          if Temp_Start.Previous = null then
            Move_Back_Cursor (List, Temp_Start, Circular);
          end if;
          Insert (List_Int, Temp_Start.Previous.Item);
          Move_Back_Cursor (List, Temp_Start, Circular);
          exit when Temp_Start = Temp_End;
        end loop;
    end case;
  end Create_Intermediate;


  function Choose_Moving_Start_Index (End_Index: in Index_Type;
                                      Destination_Index: in Index_Type;
                                      Same_List: in Boolean := False)
                                      return  Index_Type is
  --+ Overview: Return a moving index which becomes the source start index in
  --+ a generic move of a group of elements . This is necessary to allow
  --+ cursor to move in the source without removing the current element.
  begin
    case Same_List is
      when False =>
        if not (End_Index in Cursor_Type) then
          return Cursor;
        else
          case Cursor_Type (End_Index) is
            when Cursor => return Extra_Cursor;
            when Extra_Cursor => return Cursor;
          end case;
        end if;
      when True =>
        if End_Index in Cursor_Type then
          return End_Index;
        elsif Destination_Index in Cursor_Type then
          case Cursor_Type (Destination_Index) is
            when Cursor => return Extra_Cursor;
            when Extra_Cursor => return Cursor;
          end case;
        else
          return Cursor;
        end if;
    end case;
  end Choose_Moving_Start_Index;


--/ CURSOR CONSTRUCTORS :
  
  procedure Set_Cursor (List: in out List_Type;
                        Displacement: in Natural := 0;
                        From: in Index_Type := Front;
                        Direction: in Direction_Type := Next;
                        Circular: in Boolean := False;
                        This_Cursor: in Cursor_Type := Cursor) is
    Temp_Cursor: Cursor_Link_Type;
    Effective_Displacement: Natural;
  begin
    if not Is_Empty (List) then
      Init_Cursor_Position (List, From, Temp_Cursor);
      if Circular then
        Effective_Displacement := Size(List) mod Displacement;
      else
        Effective_Displacement := Displacement;
      end if;
      for Step in 1 .. Effective_Displacement loop
        case Direction is
          when Next =>
            if not Circular and then Temp_Cursor.Next = null then
              raise Cursor_Error;
            end if;
            Move_Forward_Cursor (List, Temp_Cursor, Circular);
          when Previous =>
            if not Circular and then Temp_Cursor.Previous = null then
              raise Cursor_Error;
            end if;
           Move_Back_Cursor (List, Temp_Cursor, Circular);
        end case;
      end loop;
      Put_Cursor_Link (List, This_Cursor, Temp_Cursor);
    elsif Displacement /= 0 then
      raise Cursor_Error;
    end if;
  end Set_Cursor;
  
  
  procedure Set_Cursor_At_Front (List: in out List_Type) is   
  begin
    Set_Cursor (List);
  end Set_Cursor_At_Front;
     
     
  procedure Set_Cursor_At_Rear (List: in out List_Type) is      
  begin
    Set_Cursor (List, From => Rear);
  end Set_Cursor_At_Rear;


  procedure Move_Cursor_To_Next (List: in out List_Type) is
  begin
    if List.Cursor.Next = null then
      raise Cursor_Error;
    end if;
    Move_Forward_Cursor (List, List.Cursor);
  end Move_Cursor_To_Next;
 
 
  procedure Move_Cursor_To_Previous (List: in out List_Type) is
  begin
    if List.Cursor.Previous = null then
      raise Cursor_Error;
    end if;
    Move_Back_Cursor (List, List.Cursor);
  end Move_Cursor_To_Previous;
  
  
  procedure Find_And_Set_Cursor (List: in out List_Type;
                                 Item: in Item_Type;
                                 Found: out Boolean;
                                 Start_Index: in Index_Type := Front;
                                 Direction: in Direction_Type := Next;
                                 Circular: in Boolean := False;
                                 This_Cursor: in Cursor_Type := Cursor) is
    Temp_Start, Temp_Cursor: Cursor_Link_Type;
    OK: Boolean := False;
  begin
    Found := False;
    if not Is_Empty (List) then
      Init_Cursor_Position (List, Start_Index, Temp_Cursor);
      Temp_Start := Temp_Cursor;
      case Direction is
        when Next =>
          case Circular is
            when True =>
              if Temp_Cursor.Next = null then            
                Move_Forward_Cursor (List, Temp_Cursor, Circular);
              end if;
            when False =>
              if Temp_Cursor.Next = null then            
                return;
              end if;
          end case;
          loop
            if Equals (Temp_Cursor.Next.Item, Item) then
              Found := True;
              OK := True;
              exit;
            end if;
            Move_Forward_Cursor (List, Temp_Cursor, Circular);
            case Circular is
              when True =>
                exit when Temp_Cursor = Temp_Start;
                if Temp_Cursor.Next = null then            
                  Move_Forward_Cursor (List, Temp_Cursor, Circular);
                end if;
              when False =>
                exit when Temp_Cursor.Next = null;
            end case;
          end loop;
        when Previous =>
          case Circular is
            when True =>
              if Temp_Cursor.Previous = null then     
                Move_Back_Cursor (List, Temp_Cursor, Circular);
              end if;
            when False =>
              if Temp_Cursor.Previous = null then     
                return;
              end if;
          end case;
          loop
            if Equals (Temp_Cursor.Previous.Item, Item) then
              Found := True;
              OK := True;
              exit;
            end if;
            Move_Back_Cursor (List, Temp_Cursor, Circular);
            case Circular is
              when True =>
                exit when Temp_Cursor = Temp_Start;
                if Temp_Cursor.Previous = null then     
                  Move_Back_Cursor (List, Temp_Cursor, Circular);
                end if;
              when False =>
                exit when Temp_Cursor.Previous = null;
            end case;
          end loop;
      end case;
      if OK then
        Put_Cursor_Link (List, This_Cursor, Temp_Cursor);
      end if;
    end if;
  end Find_And_Set_Cursor;


  procedure Find_And_Set_Cursor (List: in out List_Type;
                                 Item: in Item_Type;
                                 Start_Index: in Index_Type := Front;
                                 Direction: in Direction_Type := Next;
                                 Circular: in Boolean := False;
                                 This_Cursor: in Cursor_Type := Cursor) is
    Found: Boolean;
  begin
    Find_And_Set_Cursor (List, Item, Found, Start_Index, Direction, Circular,
    								This_Cursor);
    if not Found then
      raise Missing_Item_Error;
    end if;
  end Find_And_Set_Cursor;
    
    
  procedure Find_And_Set_Cursor_G (List: in out List_Type;           
                                   Found: out Boolean;
                                   Start_Index: in Index_Type := Front;
                                   Direction: in Direction_Type := Next;
                                   Circular: in Boolean := False;
                                   This_Cursor: in Cursor_Type := Cursor) is
    Temp_Start, Temp_Cursor: Cursor_Link_Type;
    OK: Boolean := False;
  begin
    if Is_Empty (List) then
      Found := False;
      return;
    end if;
    Init_Cursor_Position (List, Start_Index, Temp_Cursor);
    Temp_Start := Temp_Cursor;
    case Direction is
      when Next =>
        loop
          if Is_That_One (Temp_Cursor.Next.Item) then
            Found := True;
            OK := True;
            exit;
          end if;
          Move_Forward_Cursor (List, Temp_Cursor, Circular);
          case Circular is
            when True =>
              exit when  Temp_Cursor = Temp_Start;
              if Temp_Cursor.Next = null then                     
                Move_Forward_Cursor (List, Temp_Cursor, Circular);
              end if;
            when False =>
              exit when Temp_Cursor.Next = null;
          end case;
        end loop;
      when Previous =>
        loop
          if Is_That_One (Temp_Cursor.Previous.Item) then
            Found := True;
            OK := True;
            exit;
          end if;
          Move_Back_Cursor (List, Temp_Cursor, Circular);
          case Circular is
            when True =>
              exit when  Temp_Cursor = Temp_Start;
              if Temp_Cursor.Previous = null then              
                Move_Back_Cursor (List, Temp_Cursor, Circular);
              end if;
            when False =>
              exit when Temp_Cursor.Previous = null;
          end case;
        end loop;
    end case;
    if OK then
      Put_Cursor_Link (List, This_Cursor, Temp_Cursor);
    else
      Found := False;
    end if;
  end Find_And_Set_Cursor_G;


  procedure Find_And_Set_Cursor_With_Exception_G (List: in out List_Type;
                                     Start_Index: in Index_Type := Front;
                                     Direction: in Direction_Type := Next;
                                     Circular: in Boolean := False;
                                     This_Cursor: in Cursor_Type := Cursor) is
    Found: Boolean := False;
    procedure Find_And_Set is new Find_And_Set_Cursor_G (Is_That_One);
  begin
    Find_And_Set(List, Found, Start_Index, Direction, Circular, Cursor);
    if not Found then
      raise Missing_Item_Error;
    end if;
  end Find_And_Set_Cursor_With_Exception_G;
  

 
--/ CURSOR QUERIES :

  function Cursor_Position (List: in List_Type;
                            From: in Index_Type := Front;
                            Direction: in Direction_Type := Next;
                            This_Cursor: in Cursor_Type := Cursor)
                            return Natural is
    Temp_Cursor, Cursor_Link: Cursor_Link_Type;
    Temp_Number: Natural := 0;
  begin
    if Is_Empty (List) then
      return Temp_Number;
    end if;
    Cursor_Link := Get_Cursor_Link (List, This_Cursor);
    case From is
      when Front|Rear => 
        if Direction = Next then 
          return Cursor_Link.Position;
        else
          return List.Size - Cursor_Link.Position;
        end if;
      when Cursor|Extra_Cursor =>  
        Init_Cursor_Position (List, From, Temp_Cursor);
        while not (Temp_Cursor = Cursor_Link) loop
          Temp_Number := Temp_Number + 1;
          if Direction = Next then
            Move_Forward_Cursor (List, Temp_Cursor, True);
          else
            Move_Back_Cursor (List, Temp_Cursor, True);
          end if;
        end loop;
        return Temp_Number;
    end case;
  end Cursor_Position;

  procedure Cursor_Position (List: in List_Type;
                            Direction: out Direction_Type;
                            Displacement: out Natural;
                            From: in Index_Type := Front;
                            This_Cursor: in Cursor_Type := Cursor) is
    Temp_Cursor, Cursor_Link: Cursor_Link_Type;
    Temp_Number: Natural := 0;
    Temp_Direction: Direction_Type := Next;
  begin
    if Is_Empty (List) then
      Displacement := Temp_Number;
      Direction := Temp_Direction;
    else
      Cursor_Link := Get_Cursor_Link (List, This_Cursor);
      case From is
        when Front => 
          Displacement := Cursor_Link.Position;
          Direction := Next;
        when Rear  =>
          Displacement := List.Size - Cursor_Link.Position;
          Direction := Previous;
        when Cursor|Extra_Cursor =>        
          Init_Cursor_Position (List, From, Temp_Cursor);
          Temp_Direction := Get_Direction(List, From, Index_Type(This_Cursor));
          while not (Temp_Cursor = Cursor_Link) loop
            Temp_Number := Temp_Number + 1;
            if Temp_Direction = Next then
              Move_Forward_Cursor (List, Temp_Cursor, False);
            else
               Move_Back_Cursor (List, Temp_Cursor, False);
           end if;
          end loop;
          Displacement := Temp_Number;
          Direction := Temp_Direction;
      end case;
    end if;
  end Cursor_Position;
   

  function Is_Cursor_At_Front (List: in List_Type)
                               return Boolean is
  begin
    return List.Cursor.Next = List.First_Item_Link;
  end Is_Cursor_At_Front;


  function Is_Cursor_At_Rear (List: in List_Type)
                              return Boolean is
  begin
    return List.Cursor.Previous = List.Last_Item_Link;
  end Is_Cursor_At_Rear;

  
--/ CONSTRUCTORS :

  procedure Assign (Destination: in out List_Type;
                    Source: in List_Type) is
    Cursor_Link: Cursor_Link_Type;
  begin
    if Destination.First_Item_Link = Source.First_Item_Link then 
      return;
    end if;
    Destroy (Destination);
    if Is_Empty(Source) then
      return;
    end if;
    Init_Cursor_Position (Source, Front, Cursor_Link);
    while Cursor_Link.Next /= null loop
      Insert(Destination, Cursor_Link.Next.Item);
      Move_Forward_Cursor (Source, Cursor_Link, False);
    end loop;
  end Assign;


  procedure Construct (List: in out List_Type;
                       Items: in Array_Of_Items) is
  begin
    Destroy (List);
    for Step in Items'First .. Items'Last loop
      Insert(List, Items(Step));
    end loop;
  end Construct;

  
  procedure Copy (Destination: in out List_Type;
                  Source: in List_Type;
                  Destination_Index: in Index_Type := Cursor;
                  Source_Start_Index: in Index_Type := Front;
                  Source_Direction: in Direction_Type := Next;
                  Source_End_Index: in Index_Type := Rear;
                  Circular: in Boolean := False) is
    Temp_C_S_S, Temp_C_S_E, Temp_C_D: Cursor_Link_Type;
    Temp_S_S_I: Index_Type := Source_Start_Index;
    Temp_S_E_I: Index_Type := Source_End_Index;
    Temp_D_I: Index_Type := Destination_Index;
    Temp_Index: Index_Type;
    Temp_Source: List_Type;                              
    Temp_Direction: Direction_Type := Source_Direction;
    Between: Boolean := False;                           
  begin
    if Is_Empty (Source) then
      return;
    end if;
    Init_Cursor_Position (Source, Temp_S_S_I, Temp_C_S_S);   
    Init_Cursor_Position (Source, Temp_S_E_I, Temp_C_S_E);
    if Temp_C_S_S = Temp_C_S_E and then not Circular then
      return;
    end if;
    if Circular then
      case Source_Direction is
        when Next =>
          if Temp_C_S_S.Next = null and Temp_C_S_E.Previous = null then
            return;
          end if;
        when Previous =>
          if Temp_C_S_S.Previous = null and Temp_C_S_E.Next = null then
            return;
          end if;
      end case;
    else
      Temp_Direction := Get_Direction(Source, Temp_S_S_I, Temp_S_E_I);
    end if;
    if Destination.First_Item_Link = Source.First_Item_Link then
      if Is_Between (Source, Temp_S_S_I, Temp_S_E_I, Destination_Index,
                                                 Temp_Direction, Circular) then
        Between := True;        
        Create_Intermediate (Source, Temp_Source, Temp_S_S_I, Temp_S_E_I,
                                    Temp_Direction, Circular);     
        Temp_S_S_I := Front;
        Temp_S_E_I := Rear;
        Temp_Direction := Next;
        Init_Cursor_Position (Temp_Source, Temp_S_S_I, Temp_C_S_S);
        Init_Cursor_Position (Temp_Source, Temp_S_E_I, Temp_C_S_E);
      end if;
    end if;
    if Temp_D_I = Front then
      Temp_Index := Temp_S_E_I;
      Temp_S_S_I := Temp_S_E_I;
      Temp_S_E_I := Temp_Index;
      Temp_Direction := Other_Direction (Temp_Direction);
    end if;
    case Temp_Direction is 
      when Next =>
        loop
          if Temp_C_S_S.Next = null then                   
            if Between then
              Move_Forward_Cursor (Temp_Source, Temp_C_S_S, Circular);
            else
              Move_Forward_Cursor (Source, Temp_C_S_S, Circular);
            end if;
          end if;
          Insert (Destination, Temp_C_S_S.Next.Item, Temp_D_I);
          if Between then                            
            Move_Forward_Cursor (Temp_Source, Temp_C_S_S, Circular);
          else
            Move_Forward_Cursor (Source, Temp_C_S_S, Circular);
          end if;
          exit when Temp_C_S_S = Temp_C_S_E;
        end loop;
      when Previous =>
        loop
          if Temp_C_S_S.Previous = null then        
            if Between then
              Move_Back_Cursor (Temp_Source, Temp_C_S_S, Circular);
            else
              Move_Back_Cursor (Source, Temp_C_S_S, Circular);
            end if;
          end if;
          Insert (Destination, Temp_C_S_S.Previous.Item, Temp_D_I);
          if Between then                                   
            Move_Back_Cursor (Temp_Source, Temp_C_S_S, Circular);
          else
            Move_Back_Cursor (Source, Temp_C_S_S, Circular);
          end if;
          exit when Temp_C_S_S = Temp_C_S_E;
        end loop;
    end case;
    if Between then                                       
      Destroy (Temp_Source);
    end if;
  end Copy;
  

  procedure Set (List: in out List_Type;
                 Item: in Item_Type;
                 Index: in Index_Type := Cursor;
                 Direction: in Direction_Type := Next;
                 Circular: in Boolean := False) is
    Cursor_Link: Cursor_Link_Type;
  begin
    If Is_Empty (List) then
      raise Missing_Item_Error;
    end if;
    Init_Cursor_Position (List, Index, Cursor_Link);
    case Direction is
      when Next =>
        if Cursor_Link.Next /= null then
          Cursor_Link.Next.Item := Item;
        elsif Circular then
          List.First_Item_Link.Item := Item;
        else
          raise Missing_Item_Error;
        end if;
      when Previous =>
        if Cursor_Link.Previous /= null then
          Cursor_Link.Previous.Item := Item;
        elsif Circular then
          List.Last_Item_Link.Item := Item;
        else
          raise Missing_Item_Error;
        end if;
    end case;
  end Set;


  procedure Modify_G (List: in out List_Type;
                      Index: in Index_Type := Cursor;
                      Direction: in Direction_Type := Next;
                      Circular: in Boolean := False) is
    Cursor_Link: Cursor_Link_Type;
  begin
    If Is_Empty (List) then
      raise Missing_Item_Error;
    end if;
    Init_Cursor_Position (List, Index, Cursor_Link);
    case Direction is
      when Next =>
        if Cursor_Link.Next /= null then
          Action (Cursor_Link.Next.Item);
        elsif Circular then
          Action (List.First_Item_Link.Item);
        else
          raise Missing_Item_Error;
        end if;
      when Previous =>
        if Cursor_Link.Previous /= null then
          Action (Cursor_Link.Previous.Item);
        elsif Circular then
          Action (List.Last_Item_Link.Item);
        else
          raise Missing_Item_Error;
        end if;
    end case;
  end Modify_G;


  procedure Insert (List: in out List_Type;
                    Item: in Item_Type;
                    Index: in Index_Type := Rear) is
    Temp_Link: Link_Type := null;
    Cursor_Link: Cursor_Link_Type;
  begin
    Create_And_Assign_Cell (Temp_Link, Item);
    if Is_Empty (List) then
      List.First_Item_Link := Temp_Link;
      List.Last_Item_Link := Temp_Link;
      List.Cursor.Next := Temp_Link;
      List.Extra_Cursor.Next := Temp_Link;
    else
      case Index is
        when Front =>
          Temp_Link.Next_Item_Link := List.First_Item_Link;
          List.First_Item_Link.Previous_Item_Link := Temp_Link;
          List.First_Item_Link := Temp_Link;
        when Rear  => 
          Temp_Link.Previous_Item_Link := List.Last_Item_Link;
          List.Last_Item_Link.Next_Item_Link := Temp_Link;
          List.Last_Item_Link := Temp_Link;
        when others => 
          Cursor_Link := Get_Cursor_Link (List, Cursor_Type(Index));
          Temp_Link.Previous_Item_Link := Cursor_Link.Previous;
          if Cursor_Link.Previous /= null then
            Cursor_Link.Previous.Next_Item_Link := Temp_Link;
          else
            List.First_Item_Link := Temp_Link;
          end if;
          Temp_Link.Next_Item_Link := Cursor_Link.Next;
          if Cursor_Link.Next /= null then
            Cursor_Link.Next.Previous_Item_Link := Temp_Link;
          else
            List.Last_Item_Link := Temp_Link;
          end if;
          Cursor_Link.Previous := Temp_Link;
          Cursor_Link.Position := Cursor_Link.Position + 1;
          Put_Cursor_Link (List, Cursor_Type(Index), Cursor_Link);
      end case;
      Manage_Cursor (List, Index, Up);
    end if;
    List.Size := List.Size + 1;
  end Insert;


  procedure Remove (List: in out List_Type;
                    Index: in Index_Type := Front;
                    Direction: in Direction_Type := Next;
                    Circular: in Boolean := False) is
    Temp_Link, Temp_Next, Temp_Previous: Link_Type;
    Temp_Index: Index_Type := Index;
    Cursor_Link: Cursor_Link_Type;
  begin
    if Is_Empty (List) then
      raise Missing_Item_Error;
    end if;
    Temp_Link := Get_Link (List, Temp_Index, Direction, Circular);
    if Temp_Link = null then
      raise Missing_Item_Error;
    end if;
    Temp_Next := Temp_Link.Next_Item_Link;
    Temp_Previous := Temp_Link.Previous_Item_Link;
    if Circular then
      if Temp_Link = List.First_Item_Link then
        Temp_Index := Front;
      end if;
      if Temp_Link = List.Last_Item_Link then
        Temp_Index := Rear;
      end if;
    end if;
    List.Size := List.Size - 1;
    if List.Size = 0 then
      List := Empty_List;
    else
      if Temp_Next = null then
        List.Last_Item_Link := Temp_Previous;
        Temp_Previous.Next_Item_Link := null;
      elsif Temp_Previous = null then
        List.First_Item_Link := Temp_Next;
        Temp_Next.Previous_Item_Link := null;
      else
        Temp_Previous.Next_Item_Link := Temp_Next;
        Temp_Next.Previous_Item_Link := Temp_Previous;
      end if;
      if Temp_Index in Cursor_Type  then
        Cursor_Link := Get_Cursor_Link (List, Cursor_Type(Temp_Index));
        case Direction is
          when Previous =>
            Cursor_Link.Previous := Temp_Previous;
            Cursor_Link.Position := Cursor_Link.Position - 1;
          when Next =>
            Cursor_Link.Next := Temp_Next;
        end case;
        Put_Cursor_Link (List, Cursor_Type(Temp_Index), Cursor_Link);
      end if;
      Manage_Cursor (List, Temp_Index, Down);   
    end if;
    Release (Temp_Link);
  end Remove;

  
  procedure Remove (List: in out List_Type;
                    Item: out Item_Type;
                    Index: in Index_Type := Front;
                    Direction: in Direction_Type := Next;
                    Circular: in Boolean := False) is
  begin
    Item := Get (List, Index, Direction, Circular);
    Remove (List, Index, Direction, Circular);
  end Remove;


  procedure Swap (List: in out List_Type;
                  Index1: in Index_Type := Cursor;
                  Direction1: in Direction_Type := Next;
                  Index2: in Index_Type := Cursor;
                  Direction2: in Direction_Type := Previous;
                  Circular: in Boolean := False) is
    Temp_Link1, Temp_Next1, Temp_Prev1: Link_Type;
    Temp_Link2, Temp_Next2, Temp_Prev2: Link_Type;
    Cursor_Link: Cursor_Link_Type;
  begin
    if Is_Empty (List) then
      raise Missing_Item_Error;
    end if;
    Temp_Link1 := Get_Link (List, Index1, Direction1, Circular);
    Temp_Link2 := Get_Link (List, Index2, Direction2, Circular);
    if Temp_Link1 = null or Temp_Link2 = null then
      raise Missing_Item_Error;
    end if;
    if Temp_Link1 = Temp_Link2 then
      return;
    end if;
    Temp_Prev1 := Temp_Link1.Previous_Item_Link;
    Temp_Next1 := Temp_Link1.Next_Item_Link;
    Temp_Prev2 := Temp_Link2.Previous_Item_Link;
    Temp_Next2 := Temp_Link2.Next_Item_Link;
    if Temp_Prev1 = Temp_Link2 then
      Temp_Link1.Previous_Item_Link := Temp_Prev2;
      Temp_Link1.Next_Item_Link := Temp_Link2;
      Temp_Link2.Previous_Item_Link := Temp_Link1;
      Temp_Link2.Next_Item_Link := Temp_Next1;
    elsif Temp_Next1 = Temp_Link2 then
      Temp_Link1.Previous_Item_Link := Temp_Link2;
      Temp_Link1.Next_Item_Link := Temp_Next2;
      Temp_Link2.Previous_Item_Link := Temp_Prev1;
      Temp_Link2.Next_Item_Link := Temp_Link1;
    else
      Temp_Link1.Previous_Item_Link := Temp_Prev2;
      Temp_Link1.Next_Item_Link := Temp_Next2;
      Temp_Link2.Previous_Item_Link := Temp_Prev1;
      Temp_Link2.Next_Item_Link := Temp_Next1;
    end if;
    if Temp_Prev1 = null then
      List.First_Item_Link := Temp_Link2;
    elsif Temp_Prev1 /= Temp_Link2 then            
      Temp_Prev1.Next_Item_Link := Temp_Link2;
    end if;
    if Temp_Prev2 = null then
      List.First_Item_Link := Temp_Link1;
    else
      Temp_Prev2.Next_Item_Link := Temp_Link1;
    end if;
    if Temp_Next1 = null then
      List.Last_Item_Link := Temp_Link2;
    elsif Temp_Next1 /= Temp_Link2 then           
      Temp_Next1.Previous_Item_Link := Temp_Link2;
    end if;
    if Temp_Next2 = null then
      List.Last_Item_Link := Temp_Link1;
    else
      Temp_Next2.Previous_Item_Link := Temp_Link1;
    end if;
    for Cursor_Loop in Cursor_Type loop
      Cursor_Link := Get_Cursor_Link (List, Cursor_Loop);
      if Cursor_Link.Next = Temp_Link1 then
        Cursor_Link.Next := Temp_Link2;
      elsif Cursor_Link.Next = Temp_Link2 then
        Cursor_Link.Next := Temp_Link1;
      end if;
      if Cursor_Link.Previous = Temp_Link1 then
        Cursor_Link.Previous := Temp_Link2;
      elsif Cursor_Link.Previous = Temp_Link2 then
        Cursor_Link.Previous := Temp_Link1;
      end if;
      Put_Cursor_Link (List, Cursor_Loop, Cursor_Link);
    end loop;
  end Swap;


  procedure Move (Destination: in out List_Type;
                  Source: in out List_Type;
                  Destination_Index: in Index_Type := Cursor;
                  Source_Index: in Index_Type := Cursor;
                  Source_Direction: in Direction_Type := Next;
                  Circular: in Boolean := False) is
    Temp_Item: Item_Type;
    Cursor_Dest, Cursor_Source: Cursor_Link_Type;
  begin
    if Is_Empty (Source) then
      raise Missing_Item_Error;
    end if;
    if Destination.First_Item_Link = Source.First_Item_Link then 
      Init_Cursor_Position (Destination, Destination_Index, Cursor_Dest);
      Init_Cursor_Position (Source, Source_Index, Cursor_Source);
      if Cursor_Dest = Cursor_Source then
        return;
      end if;
    end if;
    Remove (Source, Temp_Item, Source_Index, Source_Direction, Circular);
    Insert (Destination, Temp_Item, Destination_Index);
  end Move;


  procedure Move (Destination: in out List_Type;
                  Source: in out List_Type;
                  Source_End_Index: in Index_Type;
                  Destination_Index: in Index_Type := Cursor;
                  Source_Start_Index: in Index_Type := Front;
                  Source_Direction: in Direction_Type := Next;
                  Circular: in Boolean := False) is
    Temp_C_S_S, Temp_C_S_E, Temp_C_D: Cursor_Link_Type;
    Temp_S_S_I: Index_Type := Source_Start_Index;
    Temp_S_E_I: Index_Type := Source_End_Index;
    Temp_D_I: Index_Type := Destination_Index;
    Temp_Index: Index_Type;
    Temp_Source: List_Type;                      
    Temp_Direction: Direction_Type := Source_Direction;
    Temp_Item: Item_Type;
    End_Link, Link_To_Remove: Link_Type;
    Between: Boolean := False;                       
  begin
    if Is_Empty (Source) then
      return;
    end if;
    Init_Cursor_Position (Source, Temp_S_S_I, Temp_C_S_S);  
    Init_Cursor_Position (Source, Temp_S_E_I, Temp_C_S_E);
    if Temp_C_S_S = Temp_C_S_E and then not Circular then
      return;
    end if;
    if Circular then
      case Source_Direction is
        when Next =>
          if Temp_C_S_S.Next = null and Temp_C_S_E.Previous = null then
            return;
          end if;
        when Previous =>
          if Temp_C_S_S.Previous = null and Temp_C_S_E.Next = null then
            return;
          end if;
      end case;
    else
      Temp_Direction := Get_Direction(Source, Temp_S_S_I, Temp_S_E_I);
    end if;
    if Destination.First_Item_Link = Source.First_Item_Link then
      if Is_Between (Source, Temp_S_S_I, Temp_S_E_I, Destination_Index,
                                                Temp_Direction, Circular) then
        Between := True; 
        case Circular is
          when False => 
            if Temp_Direction = Next then
              return;
            end if;
          when True => 
            if Temp_Direction = Next and not Is_Between (Source, Temp_S_S_I, 
                  Temp_S_E_I, Rear, Temp_Direction, Circular) then
              return;
            end if;
        end case;
        Create_Intermediate (Source, Temp_Source, Temp_S_S_I, Temp_S_E_I,
                                       Temp_Direction, Circular);  
        End_Link := Get_Link(Source, Temp_S_E_I, Other_Direction 
                                                   (Temp_Direction), Circular);
        loop
          Link_To_Remove := Get_Link(Source, Temp_S_S_I, Temp_Direction, 
            			       Circular);
          Remove (Source, Temp_S_S_I, Temp_Direction, Circular);
          exit when Link_To_Remove = End_Link;
        end loop;
        Temp_S_S_I := Front;
        Temp_S_E_I := Rear;
        Temp_Direction := Next;
      end if;
    end if;
    if Between then                   
      End_Link := Get_Link (Temp_Source, Temp_S_E_I, Other_Direction 
                                                   (Temp_Direction), Circular);
    else
      End_Link := Get_Link (Source, Temp_S_E_I, Other_Direction 
                                                   (Temp_Direction), Circular);
    end if;
    if Temp_D_I = Front then
      Temp_Index := Temp_S_E_I;
      Temp_S_S_I := Temp_S_E_I;
      Temp_S_E_I := Temp_Index;
      Temp_Direction := Other_Direction (Temp_Direction);
    end if;
    loop                                            
      if Between then
        Link_To_Remove := Get_Link (Temp_Source, Temp_S_S_I, Temp_Direction, 
                                                                     Circular);
        Remove (Temp_Source, Temp_Item, Temp_S_S_I, Temp_Direction, Circular);
      else
        Link_To_Remove := Get_Link (Source,Temp_S_S_I, Temp_Direction,Circular);
        Remove (Source, Temp_Item, Temp_S_S_I, Temp_Direction, Circular);
      end if;
      Insert (Destination, Temp_Item, Temp_D_I);
      exit when Link_To_Remove = End_Link;
    end loop;
  end Move;


  procedure Move_G (Destination: in out List_Type;
                    Source: in out List_Type;
                    Destination_Index: in Index_Type := Cursor;
                    Source_Start_Index: in Index_Type := Front;
                    Source_Direction: in Direction_Type := Next; 
                    Source_End_Index: in Index_Type := Rear;
                    Circular: in Boolean := False) is
    Temp_C_S_S, Temp_C_S_E, Temp_C_D: Cursor_Link_Type;
    Temp_S_S_I: Index_Type := Source_Start_Index;
    Temp_S_E_I: Index_Type := Source_End_Index;
    Temp_D_I: Index_Type := Destination_Index;
    Temp_Index: Index_Type;
    Temp_Source: List_Type;                         
    Temp_Direction: Direction_Type := Source_Direction;
    Temp_Item: Item_Type;
    End_Link, Link_To_Remove: Link_Type;
    Between: Boolean := False;                             
    Same_List: Boolean := False;
  begin
    if Is_Empty (Source) then
      return;
    end if;
    Init_Cursor_Position (Source, Temp_S_S_I, Temp_C_S_S);        
    Init_Cursor_Position (Source, Temp_S_E_I, Temp_C_S_E);
    if Temp_C_S_S = Temp_C_S_E and then not Circular then
      return;
    end if;
    if Circular then
      case Source_Direction is
        when Next =>
          if Temp_C_S_S.Next = null and Temp_C_S_E.Previous = null then
            return;
          end if;
        when Previous =>
          if Temp_C_S_S.Previous = null and Temp_C_S_E.Next = null then
            return;
          end if;
      end case;
    else
      Temp_Direction := Get_Direction(Source, Temp_S_S_I, Temp_S_E_I);
    end if;
    if Destination.First_Item_Link = Source.First_Item_Link then
      if Is_Between (Source, Temp_S_S_I, Temp_S_E_I, Destination_Index,
                                                 Temp_Direction, Circular) then
        Between := True;                              
        End_Link := Get_Link(Source, Temp_S_E_I, Other_Direction 
                                                   (Temp_Direction), Circular);
        Init_Cursor_Position (Source, Temp_S_S_I, Temp_C_S_S);
        if not (Temp_S_S_I in Cursor_Type) then
          Temp_S_S_I := Choose_Moving_Start_Index ( Temp_S_E_I, Temp_D_I);
          Put_Cursor_Link (Source, Cursor_Type (Temp_S_S_I), Temp_C_S_S);
        end if;
        loop
          Link_To_Remove := Get_Link(Source,Temp_S_S_I,Temp_Direction,Circular);
          if That_One (Link_To_Remove.Item) then
            Remove (Source, Temp_Item, Temp_S_S_I, Temp_Direction, Circular);
            Insert (Temp_Source, Temp_Item);
          else
            case Temp_Direction is
              when Next => Move_Forward_Cursor (Source, Temp_C_S_S, Circular);
              when Previous => Move_Back_Cursor (Source, Temp_C_S_S, Circular);
            end case;
            Put_Cursor_Link (Source, Cursor_Type (Temp_S_S_I), Temp_C_S_S);
          end if;
          exit when Link_To_Remove = End_Link;
        end loop;
        Temp_S_S_I := Front;
        Temp_S_E_I := Rear;
        Temp_Direction := Next;
      end if;
    end if;
    if Between then                           
      End_Link := Get_Link (Temp_Source, Temp_S_E_I, Other_Direction 
                                                   (Temp_Direction), Circular);
      Init_Cursor_Position (Temp_Source, Temp_S_S_I, Temp_C_S_S);
    else
      End_Link := Get_Link (Source, Temp_S_E_I, Other_Direction 
                                                   (Temp_Direction), Circular);
      Init_Cursor_Position (Source, Temp_S_S_I, Temp_C_S_S);
    end if;
    if Temp_D_I = Front then
      Temp_Index := Temp_S_E_I;
      Temp_S_S_I := Temp_S_E_I;
      Temp_S_E_I := Temp_Index;
      Temp_Direction := Other_Direction (Temp_Direction);
    end if;
    if not (Temp_S_S_I in Cursor_Type) then
      Temp_S_S_I := Choose_Moving_Start_Index ( Temp_S_E_I, Temp_D_I);
      Put_Cursor_Link (Source, Cursor_Type (Temp_S_S_I), Temp_C_S_S);
    end if;
    loop
      if Between then                         
        Link_To_Remove := Get_Link (Temp_Source, Temp_S_S_I, Temp_Direction, 
                                                                     Circular);
      else
        Link_To_Remove := Get_Link(Source, Temp_S_S_I, Temp_Direction,Circular);
      end if;
      if That_One (Link_To_Remove.Item) then
        if Between then                               
          Remove (Temp_Source, Temp_Item, Temp_S_S_I, Temp_Direction, Circular);
        else
          Remove (Source, Temp_Item, Temp_S_S_I, Temp_Direction, Circular);
        end if;
        Insert (Destination, Temp_Item, Temp_D_I);
      else
        case Temp_Direction is
          when Next => Move_Forward_Cursor (Source, Temp_C_S_S, Circular);
          when Previous => Move_Back_Cursor (Source, Temp_C_S_S, Circular);
        end case;
        Put_Cursor_Link (Source, Cursor_Type (Temp_S_S_I), Temp_C_S_S);
      end if;
      exit when Link_To_Remove = End_Link;
    end loop;
  end Move_G;


--/ QUERIES

  function Size (List: in List_Type) return Natural is
  begin
    return List.Size;
  end Size;


  function Is_Empty (List: in List_Type) return Boolean is
   begin
    return List.Size = 0;
  end; 

  
  function Number (List: in List_Type;
                   Item: in Item_Type;
                   Start_Index: in Index_Type := Front;
                   Direction: in Direction_Type := Next;
                   Circular: in Boolean := False)
                   return Natural is
    Cursor_Link, Cursor_Start: Cursor_Link_Type;
    Number: Natural := 0;
  begin
    if not Is_Empty (List) then       
      Init_Cursor_Position (List, Start_Index, Cursor_Start);
      if Direction = Next and Cursor_Start.Next = null then
        if Circular then
          Move_Forward_Cursor (List, Cursor_Start, Circular);
        else
          return Number;
        end if;
      elsif Direction = Previous and Cursor_Start.Previous = null then
        if Circular then
          Move_Back_Cursor (List, Cursor_Start, Circular);
        else
          return Number;
        end if;
      end if;
      Cursor_Link := Cursor_Start;
      case Direction is
        when Next =>
          loop
            if Equals (Cursor_Link.Next.Item, Item) then
              Number := Number + 1;
            end if;
            Move_Forward_Cursor (List, Cursor_Link, Circular);
            if Circular and Cursor_Link.Next = null then 
              Move_Forward_Cursor (List, Cursor_Link, Circular);
            end if;
            exit when (Circular and Cursor_Link = Cursor_Start) or
                        (not Circular and Cursor_Link.Next = null);
          end loop;
        when Previous =>
          loop
            if Equals (Cursor_Link.Previous.Item, Item) then
              Number := Number + 1;
            end if;
            Move_Back_Cursor (List, Cursor_Link, Circular);
            if Circular and Cursor_Link.Previous = null then   
              Move_Back_Cursor (List, Cursor_Link, Circular);
            end if;
            exit when (Circular and Cursor_Link = Cursor_Start) or
                    (not Circular and Cursor_Link.Previous = null); 
          end loop;
      end case;
    end if;
    return Number;
  end Number;


  function Is_Present (List: in List_Type;
                       Item: in Item_Type)
                       return Boolean is  
    Cursor_Link: Cursor_Link_Type;
    Bool: Boolean := False;
  begin
    if not Is_Empty (List) then       
      Init_Cursor_Position (List, Front, Cursor_Link);
      while not Bool and Cursor_Link.Next /= null loop
        if Equals (Cursor_Link.Next.Item, Item) then
          Bool := True;
        end if;
        Move_Forward_Cursor(List, Cursor_Link, False);
      end loop;
    end if;
    return Bool;
  end Is_Present;

   
  function Get (List: in List_Type;
                Index: in Index_Type := Cursor;
                Direction: in Direction_Type := Next;
                Circular: in Boolean := False)
                return Item_Type is
    Cursor_Link : Cursor_Link_Type;
  begin
    If Is_Empty (List) then
      raise Missing_Item_Error;
    end if;
    Init_Cursor_Position (List, Index, Cursor_Link);
    case Direction is
      when Next =>
        if Cursor_Link.Next = null then
          if Circular then
            return List.First_Item_Link.Item;
          else
            raise Missing_Item_Error;
          end if;
        else
          return Cursor_Link.Next.Item;
        end if;
      when Previous =>
        if Cursor_Link.Previous = null then
          if Circular then
            return List.Last_Item_Link.Item;
          else
            raise Missing_Item_Error;
          end if;
        else
          return Cursor_Link.Previous.Item;
        end if;
    end case;
  end Get;


  function "=" (Left, Right: in List_Type) return Boolean is
    Bool: Boolean := True;
    Cursor_Link_Left, Cursor_Link_Right: Cursor_Link_Type;
  begin
    if Size (Left) /= Size (Right) then
      return False;
    elsif Size (Left) = 0 then
      return True;
    else
      Init_Cursor_Position (Left, Front, Cursor_Link_Left);
      Init_Cursor_Position (Right, Front, Cursor_Link_Right);
      while Bool and Cursor_Link_Left.Next /= null loop
        if not Equals (Cursor_Link_Left.Next.Item, Cursor_Link_Right.Next.Item)
        then
          Bool := False; 
        end if;
        Move_Forward_Cursor (Left, Cursor_Link_Left, False);
        Move_Forward_Cursor (Right, Cursor_Link_Right, False);
      end loop;
      return Bool;
    end if;
  end "=";

  function Are_Identical (Left, Right: in List_Type) return Boolean is
  begin
    if not (Left = Right) then
      return False;
    elsif Left.Cursor.Position = Right.Cursor.Position  and
                 Left.Extra_Cursor.Position  = Right.Extra_Cursor.Position then
      return True;
    else
      return False;
    end if;
  end Are_Identical;


--/ ITERATORS

  procedure Traverse_G (List: in List_Type;
                        Start_Index: in Index_Type := Front;
                        Direction: in Direction_Type := Next;
                        Circular: in Boolean := False) is
    Cursor_Link, Cursor_Start: Cursor_Link_Type;
    Continue : Boolean := True;
  begin
    if not Is_Empty (List) then    
      Init_Cursor_Position (List, Start_Index, Cursor_Start);
      if Direction = Next and Cursor_Start.Next = null then
        if Circular then
          Move_Forward_Cursor (List, Cursor_Start, Circular);
        else
          return;
        end if;
      elsif Direction = Previous and Cursor_Start.Previous = null then
        if Circular then
          Move_Back_Cursor (List, Cursor_Start, Circular);           
        else
          return;
        end if;
      end if;
      Cursor_Link := Cursor_Start;
      case Direction is
        when Next =>
          loop
            Action (Cursor_Link.Next.Item, Continue);
            Move_Forward_Cursor (List, Cursor_Link, Circular);
            exit when not Continue;
            if Circular and Cursor_Link.Next = null then         
              Move_Forward_Cursor (List, Cursor_Link, Circular);
            end if;
            exit when (Circular and Cursor_Link = Cursor_Start) or
                       (not Circular and Cursor_Link.Next = null);  
          end loop;
        when Previous =>
          loop
            Action (Cursor_Link.Next.Item, Continue);
            Move_Back_Cursor (List, Cursor_Link, Circular);
            exit when not Continue;
            if Circular and Cursor_Link.Previous = null then      
              Move_Back_Cursor (List, Cursor_Link, Circular);
            end if;
            exit when (Circular and Cursor_Link = Cursor_Start) or
                    (not Circular and Cursor_Link.Previous = null); 
          end loop;
      end case;
    end if;
  end Traverse_G;


  procedure Traverse_Modify_G (List: in out List_Type;
                               Start_Index: in Index_Type := Front;
                               Direction: in Direction_Type := Next;
                               Circular: in Boolean := False) is
    Cursor_Link, Cursor_Start: Cursor_Link_Type;
    Continue : Boolean := True;
  begin
    if not Is_Empty (List) then         
      Init_Cursor_Position (List, Start_Index, Cursor_Start);
      if Direction = Next and Cursor_Start.Next = null then
        if Circular then
          Move_Forward_Cursor (List, Cursor_Start, Circular);         
        else
          return;
        end if;
      elsif Direction = Previous and Cursor_Start.Previous = null then
        if Circular then
          Move_Back_Cursor (List, Cursor_Start, Circular);      
        else
          return;
        end if;
      end if;
      Cursor_Link := Cursor_Start;
      case Direction is
        when Next =>
          loop
            Action (Cursor_Link.Next.Item, Continue);
            Move_Forward_Cursor (List, Cursor_Link, Circular);
            exit when not Continue;
            if Circular and Cursor_Link.Next = null then         
              Move_Forward_Cursor (List, Cursor_Link, Circular);
            end if;
            exit when (Circular and Cursor_Link = Cursor_Start) or
                       (not Circular and Cursor_Link.Next = null); 
          end loop;
        when Previous =>
          loop
            Action (Cursor_Link.Next.Item, Continue);
            Move_Back_Cursor (List, Cursor_Link, Circular);
            exit when not Continue;
            if Circular and Cursor_Link.Previous = null then      
              Move_Back_Cursor (List, Cursor_Link, Circular);
            end if;
            exit when (Circular and Cursor_Link = Cursor_Start) or
                    (not Circular and Cursor_Link.Previous = null); 
          end loop;
      end case;
    end if;
    if not continue then
      Put_Cursor_Link (List, Cursor, Cursor_Link);
    end if;
  end Traverse_Modify_G;


--/ HEAP MANAGEMENT:

  procedure Destroy (List: in out List_Type) is
    Temp_Link : Link_Type := List.First_Item_Link;
    Temp_Next : Link_Type;
  begin
    while Temp_Link /= null loop
      Temp_Next := Temp_Link.Next_Item_Link;
      Release (Temp_Link);
      Temp_Link := Temp_Next;
    end loop;
    List := Empty_List;
  end Destroy;


  procedure Release_Free_List is
    Temp_Link : Link_Type;
  begin
    while Free_List.First_Free /= null loop
      Temp_Link := Free_List.First_Free;
      Free_List.First_Free := Free_List.First_Free.Next_Item_Link;
      Dispose (Temp_Link);
    end loop;
    Free_List.Size := 0;
  end;


  procedure Set_Max_Free_List_Size (Max_Free_List_Size: in Natural) is
    Temp_Link : Link_Type;
    Diff_Number : Natural;
  begin
    if Max_Free_List_Size < Free_List.Size then
      Diff_Number := Free_List.Size - Max_Free_List_Size;
      for I in 1..Diff_Number loop
        Temp_Link := Free_List.First_Free;
        Free_List.First_Free := Free_List.First_Free.Next_Item_Link;
        Dispose (Temp_Link);
      end loop;
      Free_List.Size := Free_List.Size - Diff_Number;
    end if;
    List_Of_Static_Items_G.Max_Free_List_Size := Max_Free_List_Size;
  end Set_Max_Free_List_Size;


  function Free_List_Size return Natural is
  begin
    return Free_List.Size;
  end;

         
end List_Of_Static_Items_G;
