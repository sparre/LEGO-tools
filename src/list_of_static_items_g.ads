		  
--------------------------------- COPYRIGHT ------------------------------------
-- (C) 1993 Swiss Federal Institute of Technology (EPFL).                     --
--     Represented by A. Strohmeier EPFL-DI-LGL CH-1015 Lausanne Switzerland. --
--     All Rights Reserved.                                                   --
--------------------------------------------------------------------------------

--+ TITLE:    GENERIC PACKAGE FOR LISTS.
--+ REVISION: 16 October 1995 H. Ravaosolo, after code review
--+           Add default effect in the description of each procedure or 
--+   function. Suppress the formal parameter Cirular in the function 
--+   Cursor_Position and add a new procedure Cursor_Position that has among 
--+   others things two out parameters Direction and Position (see below the 
--+   description). Change the name of the function Are_Twins to Are_Identical. 
--+   Suppress the procedure Remove_All, it has the same behaviour as the 
--+   procedure Destroy
--+ APPROVAL: 20-August-1995 N. Mouttet
--+ REVISION: 19 June 1995 H. Ravaosolo
--+           Suppress the exception Empty_Structure_Error
--+ REVISION: 5 Avril 1995 H. Ravaosolo
--+           Change the declarations of Index_Type and Cursor_Type.
--+   Give an initial value to the formal parameter Displacement of the
--+   procedure Set_Cursor. Remove the formal parameter Number in procedures
--+   Find_And_Set_Cursor and Find_And_Set_Cursor_G. Make two declarations of
--+   Find_And_Set_Cursor, the first one has a boolean formal parameter Found
--+   the second one does not but raises the exception Missing_Item_Error if
--+   the item is not found. Same modification to Find_And_Set_Cursor_G but
--+   the two generic procedures must have differents names 
--+   (Find_And_Set_Cursor_G and Find_And_Set_Cursor_With_Exception_G).  
--+   Suppress initial value of formal parameter Source_End_Index in the second 
--+   declaration of the procedure Move. This avoid ambiguous call. Add another
--+   characteristic of Parcours_Modify_G; it allows to continue the traversing
--+   from the last break point. Remove the declaration of the array of cursors.
--+   Consequently the declarations of List_Type and the empty list are changed.
--+ REVISION: August 1994 H. Ravaosolo
--+           Add the exception Empty_Structure_Error and the constant
--+   declaration Empty_List. Change the declaration of Cursor_Link_Type.
--+   Add two formal parameters Circular and Direction to the function
--+   Cursor_Position and to the procedures Copy, Move (the second
--+   declaration) and Move_G.
--+ CREATION: 30-JUL-1993 G.Eckert.

generic
  type Item_Type is private;
  with function Equals (Left,Right: Item_Type) return Boolean;

package List_Of_Static_Items_G is
---------------------------------
--+ OVERVIEW:
--+   This package provides lists of unlimited dynamic size with elements
--+ of type Item_TYPE, where Item_TYPE is specified by a generic parameter.
--+ The type List_TYPE is implemented in such a way that every object has
--+ the implied initial value of an empty list.
--+
--+ A list appears like this:
--+
--+              element          element          element
--+                ____             ____             ____      
--+               |    |           |    |           |    |      
--+        o------|    |-----o-----|    |-----o-----|    |------o
--+               |____|           |____|           |____|       
--+      front               ^                                 rear
--+                          |
--+                <----  cursor  ---->
--+            previous              next
--+
--+   Possible points of interest (indexes) are located at the start of the
--+ list (front), at its end (rear) or on two moving cursors. Indexes never
--+ directly reference elements. They only reference inter-element locations
--+ (insertion points). Elements are referenced by specifying an index and
--+ a related direction (next or previous).
--+ In an empty list, the front and the rear collapse into a single location
--+ which is simultaneously the only valid cursor position. Every list is
--+ initially empty, with its cursors located on this position.
--+ The front and the rear also collapse when considering the list as circular.
--+ This can be done by setting the Circular parameter in the appropriate
--+ operations.
--+
--+
--+ PRIMITIVES :
--+     CURSOR CONSTRUCTORS :
--+             Set_Cursor
--+             Set_Cursor_At_Front
--+             Set_Cursor_At_Rear
--+             Move_Cursor_TO_Next
--+             Move_Cursor_TO Previous
--+             Find_And_Set_Cursor (2)
--+             Find_And_Set_Cursor_G
--+             Find_And_Set_Cursor_With_Exception_G
--+     CURSOR QUERIES :
--+             Cursor_Position (2)
--+             IS_Cursor_At_Front
--+             IS_Cursor_At_Rear
--+     LIST CONSTRUCTORS :
--+             Assign
--+             Construct
--+             Copy
--+             Set
--+             Modify
--+             Insert
--+             Remove (2)
--+             Swap
--+             Move (2)
--+             Move_G
--+     LIST QUERIES :
--+             Size
--+             Is_Empty
--+             Number
--+             Is_Present
--+             Get
--+             =
--+             Are_Identical
--+     LIST ITERATORS :
--+             Traverse_G
--+             Traverse_Modify_G
--+     HEAP MANAGEMENT :
--+    		Destroy
--+     	Release_Free_List
--+     	Set_Max_Free_List_Size
--+     	Free_List_Size
--+
--+ ALGORITHM :
--+   This component is implemented as a linear doubly linked list.
--+ An internal free list is used to avoid returning each free item (i.e.
--+ coming from Remove) to the system, so long as the length of this list
--+ does not exceed Max_Free_List_Size, in which case the free item is
--+ immediately returned to the system. When a new item has to be inserted
--+ (i.e. by a call to INSERT), an element is recovered from the free list
--+ if it is not empty. Otherwise, new space is taken from the system.


  type List_Type is limited private;

  Empty_List : constant List_Type;

  type Index_Type is (Front, Rear, Cursor, Extra_Cursor);

  subtype Cursor_Type is Index_Type range Cursor .. Extra_Cursor;
  
  type Direction_Type is (Previous, Next);
  
  Cursor_Error : exception;
  
  Missing_Item_Error : exception;


--/ CURSOR CONSTRUCTORS :
  
  procedure Set_Cursor (List: in out List_Type;   
                        Displacement: in Natural := 0;
                        From: in Index_Type := Front;
                        Direction: in Direction_Type := Next;
                        Circular: in Boolean := False;
                        This_Cursor: in Cursor_Type := Cursor);
  --+ OVERVIEW:
  --+   This_Cursor of List is moved by Displacement items in the given 
  --+ Direction, starting at From index.
  --+ If Circular is True then the front and the rear collapse into a single 
  --+ point. 
  --+ With default parameters, no action is taken because the Displacement value
  --+ is zero.
  --+ ERRORS:
  --+   The exception Cursor_Error is raised if the end position of Cursor
  --+ is not located between the front and the rear of List. If List is not 
  --+ empty and Circular then this is never the case.
  
  procedure Set_Cursor_At_Front (List: in out List_Type);
  --+ OVERVIEW:
  --+   The Cursor of List is moved to the front. No error occurs if List is
  --+ empty.
     
  procedure Set_Cursor_At_Rear (List: in out List_Type);
  --+ OVERVIEW:
  --+   The Cursor of List is moved to the rear. No error occurs if List is
  --+ empty.

   
  procedure Move_Cursor_To_Next (List: in out List_Type);
  --+ OVERVIEW:
  --+   The Cursor of List is moved by one item in the Next direction.
  --+ ERROR:
  --+   The exception Cursor_Error is raised if the end position is not
  --+ located between the front and the rear of List. 
 
  procedure Move_Cursor_To_Previous (List: in out List_Type);
  --+ OVERVIEW:
  --+   The Cursor of List is moved by one item in the Previous
  --+ direction.
  --+ ERROR:
  --+   The exception Cursor_Error is raised if the end position is not
  --+ located between the front and the rear of List. 
  
  procedure Find_And_Set_Cursor (List: in out List_Type;
                                 Item: in Item_Type;
                                 Found: out Boolean;
                                 Start_Index: in Index_Type := Front;
                                 Direction: in Direction_Type := Next;
                                 Circular: in Boolean := False;
                                 This_Cursor: in Cursor_Type := Cursor);
  --+ OVERVIEW:
  --+   Set This_Cursor on a location such as there is in Direction the first 
  --+ Item found in List when starting the search at Start_Index and searching 
  --+ in the given Direction.
  --+ If List is Circular, the whole list is searched. Else, the search stops
  --+ at the rear (if Direction is Next) or at the front (if Direction is
  --+ Previous).
  --+ No action is taken if Item is not in List, except that FOUND is set to
  --+ false.
  --+ With default parameters, the whole list is searched from the front and
  --+ the working cursor is Cursor

  procedure Find_And_Set_Cursor (List: in out List_Type;
                                 Item: in Item_Type;
                                 Start_Index: in Index_Type := Front;
                                 Direction: in Direction_Type := Next;
                                 Circular: in Boolean := False;
                                 This_Cursor: in Cursor_Type := Cursor);
  --+ OVERVIEW:
  --+   Set This_Cursor on a location such as there is in Direction the first 
  --+ Item found in List when starting the search at Start_Index and search
  --+ in the given Direction.
  --+ If List is Circular, the whole list is searched. Else, the search stops
  --+ at the rear (if Direction is Next) or at the front (if Direction is
  --+ Previous).
  --+ With default parameters, the whole list is searched from the front and
  --+ the working cursor is Cursor
  --+ ERROR:
  --+   The exception Missing_Item_Error is raised if Item is not in List.

  generic                                                
    with function Is_That_One (Item: in Item_Type)
                               return Boolean;
  procedure Find_And_Set_Cursor_G (List: in out List_Type;
                                   Found: out Boolean;
                                   Start_Index: in Index_Type := Front;
                                   Direction: in Direction_Type := Next;
                                   Circular: in Boolean := False;
                                   This_Cursor: in Cursor_Type := Cursor);
  --+ OVERVIEW:
  --+   Set This_Cursor on a location such as there is in Direction the first
  --+ Item in List satisfying the condition given by the function Is_That_One.
  --+ The search starts at Start_Index and goes in the given Direction.
  --+ If List is Circular, the whole list is searched. Else, the search stops
  --+ at the rear (if Direction is Next) or at the front (if Direction is
  --+ Previous).
  --+ No action is taken if any Item in List satisfy the condition, except
  --+ that FOUND is set to false. 
  --+ With default parameters, the whole list is searched from the front and
  --+ the working cursor is Cursor

  generic        
    with function Is_That_One (Item: in Item_Type)
                               return Boolean;
  procedure Find_And_Set_Cursor_With_Exception_G (List: in out List_Type;
                                       Start_Index: in Index_Type := Front;
                                       Direction: in Direction_Type := Next;
                                       Circular: in Boolean := False;
                                       This_Cursor: in Cursor_Type := Cursor);
  --+ OVERVIEW:
  --+   Set This_Cursor on a location such as there is in Direction the first
  --+ Item in List satisfying the condition given by the function Is_That_One.
  --+ The search starts at Start_Index and goes in the given Direction.
  --+ If List is Circular, the whole list is searched. Else, the searching 
  --+ stops at the rear (if Direction is Next) or at the front (if Direction 
  --+ is Previous).
  --+ With default parameters, the whole list is searched from the front and
  --+ the working cursor is Cursor
  --+ ERROR:
  --+   The exception Missing_Item_Error is raised in any Item in the List
  --+ satisfy the condition.
  
  
--/ CURSOR QUERIES :

  function Cursor_Position (List: in List_Type;
                            From: in Index_Type := Front;
                            Direction: in Direction_Type := Next;
                            This_Cursor: in Cursor_Type := Cursor)
                            return Natural;
  --+ OVERVIEW:
  --+   Returns the number of items located in List between the From index
  --+ and This_Cursor in the given Direction. List is considered as circular.
  --+ But it can be used to get the position of a cursor in relation to the 
  --+ front of a non-circular list.
  --+ If List is empty then the returned value is 0.
  --+ With default parameters, the position of Cursor in relation to Front in
  --+ the next direction is returned

  procedure Cursor_Position (List: in List_Type;
                             Direction: out Direction_Type;
 			     Displacement: out Natural;			     
 			     From: in Index_Type := Front;
                             This_Cursor: in Cursor_Type := Cursor);
  --+ OVERVIEW:
  --+   Provides the number of items located in List between the From index
  --+ and the given cursor This_Cursor into Displacement, and the related 
  --+ direction into Direction. The List is not considered as circular. It
  --+ is especially used to locate a cursor in relation to an index. If List is 
  --+ empty then the Direction is Next and the Displacement value is 0.
  --+ With default parameters, Direction receives the value Next and the 
  --+ Displacement the position of Cursor in relation to Front in the next
  --+ direction is returned


  function Is_Cursor_At_Front (List: in List_Type)
                               return Boolean;
  --+ OVERVIEW:
  --+   Returns True if Cursor of List is located at the front.

  function Is_Cursor_At_Rear (List: in List_Type)
                              return Boolean;
  --+ OVERVIEW:
  --+   Returns True if Cursor of List is located at the rear.

  
--/ CONSTRUCTORS :

  procedure Assign (Destination: in out List_Type;
                    Source: in List_Type);
  --+ OVERVIEW:
  --+   Copies the list Source into the list Destination. If Destination is
  --+ not empty, its contents are overwritten.


  type Array_Of_Items is array (Positive range <>) of Item_Type;

  procedure Construct (List: in out List_Type;
                       Items: in Array_Of_Items); 
  --+ OVERVIEW:
  --+   Given an array of Item_TYPE, constructs List by copying these
  --+ elements. If List is not empty, its contents are overwritten.
  --+ This procedure allows the use of aggregate like
  --+     Construct (List, (4, 8, 6, 2, 9));


  procedure Copy (Destination: in out List_Type;
                  Source: in List_Type;
                  Destination_Index: in Index_Type := Cursor;
                  Source_Start_Index: in Index_Type := Front;
                  Source_Direction: in Direction_Type := Next;
                  Source_End_Index: in Index_Type := Rear;
                  Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Copies the items of Source between Source_Start_Index and
  --+ Source_End_Index in the given direction Source_Direction and insert them 
  --+ into Destination at the given Destination_Index.
  --+   There are no restrictions in the relative location of the three
  --+ indexes: they may even all be index of the same list. Therefore,
  --+ this operation may be used to duplicate or reverse sublists
  --+    If Circular is False, the direction is calculated according to 
  --+ Source_End_Index position in relation to Source_Start_Index. 
  --+ Source_Direction is not considered.
  --+   if Destination_Index denotes a cursor, it will be moved to some 
  --+ positions (number of copied items) in the next direction (consequence of
  --+ Insert).
  --+   With default parameters, the whole Source is copied into Destination at
  --+ the position of Cursor.


  procedure Set (List: in out List_Type;
                 Item: in Item_Type;
                 Index: in Index_Type := Cursor;
                 Direction: in Direction_Type := Next;
                 Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Replaces the item of List referenced by Index and Direction with
  --+ Item.
  --+   With default parameters, the item of List at the next side of Cursor is
  --+ changed.
  --+ ERROR:
  --+   The exception Missing_Item_Error is raised if there is no item in
  --+ the given Direction. If List is not empty and Circular then this is 
  --+ never the case.

  generic
    with procedure Action (Item: in out Item_Type);
  procedure Modify_G (List: in out List_Type;
                      Index: in Index_Type := Cursor;
                      Direction: in Direction_Type := Next;
                      Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Applies procedure Action on the item referenced by Index and Direction.
  --+   With default parameters, Action is applied on the item of List at the 
  --+ next side of Cursor.
  --+ ERROR:
  --+   The exception Missing_Item_Error is raised if there is no item in
  --+ the given Direction. If List is not empty and Circular then this is 
  --+ never the case.

  procedure Insert (List: in out List_Type;
                    Item: in Item_Type;
                    Index: in Index_Type := Rear);
  --+ OVERVIEW:
  --+   Inserts Item at the given Index location in List. After the insertion, 
  --+ if Index denotes a cursor, it will be moved to one position in the next 
  --+ direction.
  --+   With default parameters, Item is inserted at the end of List.
  
  procedure Remove (List: in out List_Type;
                    Index: in Index_Type := Front;
                    Direction: in Direction_Type := Next;
                    Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Removes the item of List referenced by Index and Direction.
  --+   With default parameters, the item at the front of List is removed.
  --+ ERROR:
  --+   The exception Missing_Item_Error is raised if there is no item in
  --+ the given Direction or List is empty. If List is not empty and Circular,  
  --+ this is never the case.
  
  procedure Remove (List: in out List_Type;
                    Item: out Item_Type;
                    Index: in Index_Type := Front;
                    Direction: in Direction_Type := Next;
                    Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Removes the item of List referenced by Index and Direction and
  --+ returns it in Item.
  --+   With default parameters, the item at the front of List is removed.
  --+ ERROR:
  --+   The exception Missing_Item_Error is raised if there is no item in
  --+ the given Direction or List is empty. If List is not empty and Circular,
  --+ this is never the case.

  procedure Swap (List: in out List_Type;
                  Index1: in Index_Type := Cursor;
                  Direction1: in Direction_Type := Next;
                  Index2: in Index_Type := Cursor;
                  Direction2: in Direction_Type := Previous;
                  Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Exchanges the item of List referenced by Index1 and Direction1 with
  --+ the item referenced by Index2 and Direction2.
  --+   No action is taken if the combinations Index1-Direction1 and 
  --+ Index2-Direction2 point to the same item.
  --+ With default parameters, this procedure swaps the two items at the both 
  --+ sides of Cursor.
  --+ ERRORS:
  --+   The exception Missing_Item_Error is raised if either of the referenced
  --+   item is missing in the given Direction or List is empty. If List is
  --+   not empty and Circular then this is never the case.

  procedure Move (Destination: in out List_Type;
                  Source: in out List_Type;
                  Destination_Index: in Index_Type := Cursor;
                  Source_Index: in Index_Type := Cursor;
                  Source_Direction: in Direction_Type := Next;
                  Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Removes the item of Source referenced by Source_Index and 
  --+ Source_Direction, and insert it in Destination at the location referenced 
  --+ by Destination_Index.
  --+   There are no restrictions in the relative location of the two
  --+ indexes: they may even both be index of the same list. Therefore,
  --+ this operation may be used to rearrange a list.
  --+ ERROR:
  --+   The exception Missing_Item_Error is raised if there is no Source
  --+ item in the given Source_Direction. If Source is not empty and Circular, 
  --+ then this is never the case.
  
  procedure Move (Destination: in out List_Type;
                  Source: in out List_Type;
                  Source_End_Index: in Index_Type;
                  Destination_Index: in Index_Type := Cursor;
                  Source_Start_Index: in Index_Type := Front;
                  Source_Direction: in Direction_Type := Next;
                  Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Removes the items of Source located between Source_Start_Index and
  --+ Source_End_Index in the given direction Source_Direction, and insert them 
  --+ at the given destination index.
  --+   There are no restrictions in the relative location of the three
  --+ indexes: they may even all be index of the same list. Therefore,
  --+ this operation may be used to move or reverse sublists.
  --+   Note that all destination positions located between the source
  --+ indexes collapse into a single point when considering the same
  --+ list as source and destination.
  --+   If Circular is False, the direction is calculated according to 
  --+ Source_End_Index position in relation to Source_Start_Index. 
  --+ Source_Direction is not considered.
  --+   if Destination_Index denotes a cursor, it will be moved to some 
  --+ positions (number of moved items) in the next direction (consequence of
  --+ Insert).
  --+   With default parameters, all items of Source between its front and the
  --+ Source_End_Index is copied into Destination at the position of Cursor.


  generic
    with function That_One (Item: in Item_Type)
                            return Boolean;
  procedure Move_G (Destination: in out List_Type;
                    Source: in out List_Type;
                    Destination_Index: in Index_Type := Cursor;
                    Source_Start_Index: in Index_Type := Front;
                    Source_Direction: in Direction_Type := Next; 
                    Source_End_Index: in Index_Type := Rear;
                    Circular: in Boolean := False);
  --+ OVERVIEW:
  --+   Removes the items of Source located between Source_Start_Index
  --+ and Source_End_Index for which the condition given by the function
  --+ That_One is satisfied, and insert them at the given destination index.
  --+   There are no restrictions in the relative location of the three
  --+ indexes: they may even all be index of the same list. Therefore,
  --+ this operation may be used to move or reverse sublists.
  --+   Note that all destination positions located between the source
  --+ indexes collapse into a single point when considering the same
  --+ list as source and destination.
  --+   If Circular is False, the direction is calculated according to 
  --+ Source_End_Index position in relation to Source_Start_Index. 
  --+ Source_Direction is not considered.
  --+   if Destination_Index denotes a cursor, it will be moved to some 
  --+ positions (number of moved items) in the next direction (consequence of
  --+ Insert).
  --+   The cursor that corresponds to the Source_Start_Index may be moved if
  --+ there are items that don't satify the given condition. If this index is
  --+ not a cursor then one of two cursors would be choosen and consequently
  --+ woul be moved.
  --+   With default parameters, the whole Source is tested and the appropriate
  --+ items are copied into Destination at the position of Cursor.


--/ QUERIES

  function Size (List: in List_Type) return Natural;
  --+ OVERVIEW:
  --+   Returns the number of items currently in List.
  
  function Is_Empty (List: in List_Type) return Boolean;
  --+ OVERVIEW:
  --+   Returns True if List is empty, else returns FALSE;
  
  function Number (List: in List_Type;                             
                   Item: in Item_Type;
                   Start_Index: in Index_Type := Front;
                   Direction: in Direction_Type := Next;
                   Circular: in Boolean := False)
                   return Natural;
  --+ OVERVIEW:
  --+   Returns the number of Item found in List when starting the
  --+ search at Start_Index and searching in the given Direction.
  --+   If List is Circular, the whole list is searched. Else, the searching   
  --+ stops at the rear (if Direction is Next) or at the front (if Direction
  --+ is Previous).
  --+   With default parameters, the whole List is visited.

  function Is_Present (List: in List_Type;                         
                       Item: in Item_Type)
                       return Boolean;
  --+ OVERVIEW:
  --+   Returns True if Item is present (at least once) in List, else
  --+ returns FALSE; 
   
  function Get (List: in List_Type;
                Index: in Index_Type := Cursor;
                Direction: in Direction_Type := Next;
                Circular: in Boolean := False)
                return Item_Type;
  --+ OVERVIEW:
  --+   Returns a copy of the item referenced by Index and Direction in List.
  --+   With default parameters, a copy of the item at the next side of Cursor
  --+   is returned.
  --+ ERRORS:
  --+   The exception Missing_Item_Error is raised if there is no item in
  --+ the given Direction. If List is not empty and Circular then this is
  --+ never the case.

  function "=" (Left, Right: in List_Type) return Boolean;
  --+ OVERVIEW:
  --+   Returns True if both lists are equal, which means that every 
  --+ element of one list is found at the same position in the other one
  --+ (the location of the cursors doesn't matter). Else returns FALSE.
  --+ Two empty lists are considered as equal.

  function Are_Identical (Left, Right: in List_Type) return Boolean;
  --+ OVERVIEW:
  --+   Returns True if both list are equal (as defined by the "=" operation)
  --+ and have pairwise identical positions for both cursors. Returns True
  --+ if both list are empty.


--/ ITERATORS

  generic 
    with procedure Action (Item: in Item_Type;
                           Continue: in out Boolean) is <>;
  procedure Traverse_G (List: in List_Type;
                        Start_Index: in Index_Type := Front;
                        Direction: in Direction_Type := Next;
                        Circular: in Boolean := False);
  --+ OVERVIEW :
  --+   Applies procedure Action on each Item of List, starting from the
  --+ item referenced by Start_Index and Direction. The list is traversed
  --+ in the given Direction. 
  --+   The boolean parameter Continue specifies if you want to proceed to
  --+ the next (or previous) item or if you want to stop the traversal.
  --+ As long as you do not modify its value within Action, its value
  --+ remains true.
  --+   If List is Circular, the whole list is traversed. Else, the traversal   
  --+ stops at the rear (if Direction is Next) or at the front (if Direction 
  --+ is Previous).
  --+ REQUIREMENT:
  --+   The actual procedure Action must not modify the traversed List.

  generic 
    with procedure Action (Item: in out Item_Type;
                           Continue: in out Boolean) is <>;
  procedure Traverse_Modify_G (List: in out List_Type;
                               Start_Index: in Index_Type := Front;
                               Direction: in Direction_Type := Next;
                               Circular: in Boolean := False);
  --+ OVERVIEW :
  --+   Applies procedure Action on each Item of List, starting from the
  --+ item referenced by Start_Index and Direction. The list is traversed
  --+ in the given Direction. 
  --+   The boolean parameter Continue specifies if you want to proceed to
  --+ the next (or previous) item or if you want to stop traversal.
  --+ As long as you do not modify its value within Action, its value
  --+ remains true.
  --+   If List is Circular, the whole list is traversed. Else, the traversal  
  --+ stops at the rear (if Direction is Next) or at the front (if Direction 
  --+ is Previous).
  --+ If the traversal is stopped, the cursor Cursor is set at the next (if
  --+ Direction is Next) or previous (if Direction is Previous) side of the
  --+ last treated item. This allows to start again the traversing from the
  --+ break point by using the same direction and Cursor as Start_Index value.


--/ HEAP MANAGEMENT:

  procedure Destroy (List: in out List_Type);
  --+ OVERVIEW:
  --+   Empties the List and returns space to the free list

  procedure Release_Free_List;
  --+ OVERVIEW:
  --+   Releases all items from the free list giving their space back to the
  --+ system.

  procedure Set_Max_Free_List_Size (Max_Free_List_Size: in Natural);
  --+ OVERVIEW:
  --+   Sets the maximum length of the free list which is 0 by default. If
  --+ parameter Max_Free_List_Size is smaller than the current size of the
  --+ list, the items in excess are returned to the system.

  function Free_List_Size return Natural;
  --+ OVERVIEW:
  --+   Returns the actual length of the free list.


private

  type Cell_Type;
  
  type Link_Type is access Cell_Type;
  
  type Cell_Type is
         record
           Item: Item_Type;
           Next_Item_Link: Link_Type := null;
           Previous_Item_Link: Link_Type := null;
         end record;

  type Cursor_Link_Type is
         record
           Previous: Link_Type := null;
           Next: Link_Type := null;
           Position: Natural := 0;
         end record;

          
  type List_Type is 
         record
           First_Item_Link: Link_Type := null;
           Last_Item_Link: Link_Type := null;
           Cursor: Cursor_Link_Type;
           Extra_Cursor: Cursor_Link_Type;
           Size: Natural := 0;
         end record;
         
  Empty_List : constant List_Type := (null, 
                                      null, 
                                      (null, null, 0),
                                      (null, null, 0),
                                      0);
         
end List_Of_Static_Items_G;
