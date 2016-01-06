package body Integer_Intervals is

   function Get_Intervals (First                          : Integer;
                           Last                           : Integer;
                           Number_Of_Parts_To_Divide_Into : Positive)
                           return Interval_Collection_Type
   is
      Interval_Length : constant Integer := (Last - First)/Number_Of_Parts_To_Divide_Into;

      Interval : Interval_Type;
      Interval_Collection : Interval_Collection_Type;

      F : Integer := First;
      L : Integer := First + Interval_Length;
   begin
      for I in Integer range 1..(Number_Of_Parts_To_Divide_Into - 1) loop
         Interval.First := F;
         Interval.Last  := L;
         Interval_Collection.Append (Interval);

         F := L + 1;
         L := F + Interval_Length;
      end loop;

      Interval.First := F;
      Interval.Last  := Last;
      Interval_Collection.Append (Interval);

      return Interval_Collection;
   end Get_Intervals;

end Integer_Intervals;
