with Ada.Containers.Vectors;

package Integer_Intervals is

   type Interval_Type is
      record
         First : Integer;
         Last  : Integer;
      end record;

   package Interval_Collection_Type_Owner is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                         Element_Type => Interval_Type);

   subtype Interval_Collection_Type is Interval_Collection_Type_Owner.Vector;
   subtype Interval_Cursor_Type     is Interval_Collection_Type_Owner.Cursor;

   function Get_Intervals (First                          : Integer;
                           Last                           : Integer;
                           Number_Of_Parts_To_Divide_Into : Positive) return Interval_Collection_Type;

end Integer_Intervals;
