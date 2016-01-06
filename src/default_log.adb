with Ada.Text_IO;

package body Default_Log is

   protected body Synchronized_Text_IO is

      procedure Put_Line (Item : String)
      is
      begin
         Ada.Text_IO.Put_Line(Item);
      end Put_Line;

   end Synchronized_Text_IO;

end Default_Log;
