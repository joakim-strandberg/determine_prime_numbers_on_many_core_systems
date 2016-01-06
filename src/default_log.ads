package Default_Log is

   protected Synchronized_Text_IO is

      procedure Put_Line (Item : String) with
        Global => null;

   end Synchronized_Text_IO;

end Default_Log;
