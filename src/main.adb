with Generic_Tasking;
with Ada.Text_IO;
with Default_Log;
with Generic_Prime_Numbers;
with Ada.Real_Time;
with Object.Handle;
with Integer_Intervals;
with System.Multiprocessors;

procedure Main is

   use type System.Multiprocessors.CPU_Range;

   type Parameterless_Subprogram_Access_Type is access procedure;

   function Measure_Time (Method : Parameterless_Subprogram_Access_Type) return Duration
   is
      Start_Time_Stamp : Ada.Real_Time.Time;
      End_Time_Stamp   : Ada.Real_Time.Time;
   begin
      Start_Time_Stamp := Ada.Real_Time.Clock;
      Method.all;
      End_Time_Stamp := Ada.Real_Time.Clock;

      declare
         use type Ada.Real_Time.Time;

         Total_Time : constant Duration := Ada.Real_Time.To_Duration (End_Time_Stamp - Start_Time_Stamp);
      begin
         return Total_Time;
         --         Ada.Text_IO.Put_Line ("Duration: " & Total_Time'Img);
      end;
   end Measure_Time;

   Max_Prime_Number : constant Positive := 64_000_000;
--   Max_Prime_Number : constant Positive := 1_000;

   package Natural_Prime_Numbers is new Generic_Prime_Numbers (Natural, 0, 1, 2);

   End_Time_Stamp_When_Multi_Tasking : Ada.Real_Time.Time;

   type Number_To_Is_Prime_Number_Array_Type is array (Positive range <>) of Boolean;

   type Number_To_Is_Prime_Number_Array_Entity_Type (First : Positive;
                                                     Last  : Positive) is new Object.Entity with
      record
         Number_To_Is_Prime_Number_Array : Number_To_Is_Prime_Number_Array_Type (First..Last);
      end record;

   type Number_To_Is_Prime_Number_Array_Entity_Access_Type is access all Number_To_Is_Prime_Number_Array_Entity_Type'Class;

   package Number_To_Is_Prime_Number_Array_Entity_Handle is new Object.Handle (Object_Type     => Number_To_Is_Prime_Number_Array_Entity_Type,
                                                                               Object_Type_Ptr => Number_To_Is_Prime_Number_Array_Entity_Access_Type);

   Correct_Number_To_Is_Prime_Number_Array_Handle : Number_To_Is_Prime_Number_Array_Entity_Handle.Handle;

   type Correct_Number_To_Is_Prime_Number_Array_Entity_Type is new Number_To_Is_Prime_Number_Array_Entity_Type (First => 1,
                                                                                                                Last  => Max_Prime_Number) with null record;
   procedure Determine_Prime_Numbers_Multi_Tasking (Number_Of_Worker_Tasks : Positive) is

      procedure Internal_Determine_Prime_Numbers_Multi_Tasking is

         Max_Queue_Index : constant Positive := Number_Of_Worker_Tasks + 1;

         subtype Queue_Index_Type is Positive range 2..Max_Queue_Index;

         subtype Task_Id_Type is Positive range 1..Max_Queue_Index;

         package Tasking is new Generic_Tasking (Queue_Index_Type     => Queue_Index_Type,
                                                 Task_Id_Type         => Task_Id_Type,
                                                 Handle_Error_Message => Default_Log.Synchronized_Text_IO.Put_Line'Unrestricted_Access);

         Queues : aliased Tasking.Tasks.Array_Of_Queues_Type;

         Worker_Tasks : array (Tasking.Worker_Task_Id_Type'Range) of aliased Tasking.Tasks.Worker_Task;

         Command_Executors : array (Queue_Index_Type) of aliased Tasking.Tasks.Command_Executor_Type;

         --
         --
         --

         package Commands is

            type State_Initializer_Type (<>) is new Tasking.Model.Command_With_State_Initializer_Type with private;

            function Make (State_Id                    : Tasking.Model.Fields.State_Id_As_64_Bits_Type;
                           Work_Package_Id             : Tasking.Model.Fields.Work_Package_Identifer_Type;
                           Task_Responsible_For_Result : Tasking.Worker_Task_Id_Type) return State_Initializer_Type;

            overriding
            function Entity_Handle (This : State_Initializer_Type) return Tasking.Entity_Handle.Handle;

            type Determine_Whether_Prime_Or_Not_Command_Type (<>) is new Tasking.Model.Command_Type with private;

            function Make (Interval                    : Integer_Intervals.Interval_Type;
                           State_Id                    : Tasking.Model.Fields.State_Id_As_64_Bits_Type;
                           Work_Package_Id             : Tasking.Model.Fields.Work_Package_Identifer_Type;
                           Task_Responsible_For_Result : Tasking.Worker_Task_Id_Type) return Determine_Whether_Prime_Or_Not_Command_Type;

            overriding
            procedure Execute (This                                 : Determine_Whether_Prime_Or_Not_Command_Type;
                               Queues                               : Tasking.Queues_Access_Type;
                               Report_Command_Successfully_Executed : Tasking.Procedure_With_Zero_Parameters_Access_Type);

            type Append_Partial_Result_Command_Type (<>) is new Tasking.Model.Command_With_State_Type with private;

            function Make (Handle                      : Number_To_Is_Prime_Number_Array_Entity_Handle.Handle;
                           State_Id                    : Tasking.Model.Fields.State_Id_As_64_Bits_Type;
                           Work_Package_Id             : Tasking.Model.Fields.Work_Package_Identifer_Type;
                           Task_Responsible_For_Result : Tasking.Worker_Task_Id_Type) return Append_Partial_Result_Command_Type;

            overriding
            procedure Execute (This   : Append_Partial_Result_Command_Type;
                               Queues : Tasking.Queues_Access_Type;
                               Entity : in out Object.Entity'Class;
                               Report_Command_Successfully_Executed : Tasking.Procedure_With_Zero_Parameters_Access_Type);

            overriding
            procedure Execute_When_Finished (This   : Append_Partial_Result_Command_Type;
                                             Queues : Tasking.Queues_Access_Type;
                                             Entity : in out Object.Entity'Class);

         private

            type State_Initializer_Type (State_Id : Tasking.Model.Fields.State_Id_As_64_Bits_Type) is new Tasking.Model.Command_With_State_Initializer_Type (State_Id) with null record;

            type Determine_Whether_Prime_Or_Not_Command_Type is new Tasking.Model.Command_Type with
               record
                  Interval : Integer_Intervals.Interval_Type;
                  State_Id : Tasking.Model.Fields.State_Id_As_64_Bits_Type;
               end record;

            type Append_Partial_Result_Command_Type is new Tasking.Model.Command_With_State_Type with
               record
                  Handle : Number_To_Is_Prime_Number_Array_Entity_Handle.Handle;
               end record;

         end Commands;

         package body Commands is

            function Make (State_Id                    : Tasking.Model.Fields.State_Id_As_64_Bits_Type;
                           Work_Package_Id             : Tasking.Model.Fields.Work_Package_Identifer_Type;
                           Task_Responsible_For_Result : Tasking.Worker_Task_Id_Type) return State_Initializer_Type
            is
               State_Initializer : State_Initializer_Type (State_Id);
            begin
               State_Initializer.Initialize (Work_Package_Id             => Work_Package_Id,
                                             Task_Responsible_For_Result => Task_Responsible_For_Result);
               return State_Initializer;
            end Make;

            function Entity_Handle (This : State_Initializer_Type) return Tasking.Entity_Handle.Handle is
               pragma Unreferenced (This);
               H : Tasking.Entity_Handle.Handle;
               Number_To_Is_Prime_Number_Array_Entity : constant Number_To_Is_Prime_Number_Array_Entity_Access_Type := new Correct_Number_To_Is_Prime_Number_Array_Entity_Type;
            begin
               H.Set (Object.Entity (Number_To_Is_Prime_Number_Array_Entity.all)'Unchecked_Access);
               return H;
            end Entity_Handle;

            function Make (Interval                    : Integer_Intervals.Interval_Type;
                           State_Id                    : Tasking.Model.Fields.State_Id_As_64_Bits_Type;
                           Work_Package_Id             : Tasking.Model.Fields.Work_Package_Identifer_Type;
                           Task_Responsible_For_Result : Tasking.Worker_Task_Id_Type) return Determine_Whether_Prime_Or_Not_Command_Type is
               C : Determine_Whether_Prime_Or_Not_Command_Type;
            begin
               C.Interval := Interval;
               C.State_Id := State_Id;
               C.Initialize (Work_Package_Id,
                             Task_Responsible_For_Result);
               return C;
            end Make;

            procedure Execute (This                                 : Determine_Whether_Prime_Or_Not_Command_Type;
                               Queues                               : Tasking.Queues_Access_Type;
                               Report_Command_Successfully_Executed : Tasking.Procedure_With_Zero_Parameters_Access_Type)
            is
               pragma Unreferenced (Report_Command_Successfully_Executed);

               H : Number_To_Is_Prime_Number_Array_Entity_Handle.Handle;
               Number_To_Is_Prime_Number_Array_Entity : constant Number_To_Is_Prime_Number_Array_Entity_Access_Type := new Number_To_Is_Prime_Number_Array_Entity_Type (This.Interval.First,
                                                                                                                                                                        This.Interval.Last);
            begin
               H.Set (Number_To_Is_Prime_Number_Array_Entity);

               for I in Number_To_Is_Prime_Number_Array_Entity.Number_To_Is_Prime_Number_Array'Range loop
                  Number_To_Is_Prime_Number_Array_Entity.Number_To_Is_Prime_Number_Array (I) := Natural_Prime_Numbers.Is_Prime (I);
               end loop;

               declare
                  Partial_Result : constant Append_Partial_Result_Command_Type := Make (Handle                      => H,
                                                                                        State_Id                    => This.State_Id,
                                                                                        Work_Package_Id             => This.Work_Package_Id,
                                                                                        Task_Responsible_For_Result => This.Task_Responsible_For_Result);
                  Command : Tasking.Containers.Command_Holder_Type (Tasking.Model.Custom_Command_With_State);
               begin
                  Command.Custom_Command_With_State.Replace_Element (Partial_Result);
                  Queues (This.Task_Responsible_For_Result).Enqueue (Command);
               end;
            end Execute;

            function Make (Handle                      : Number_To_Is_Prime_Number_Array_Entity_Handle.Handle;
                           State_Id                    : Tasking.Model.Fields.State_Id_As_64_Bits_Type;
                           Work_Package_Id             : Tasking.Model.Fields.Work_Package_Identifer_Type;
                           Task_Responsible_For_Result : Tasking.Worker_Task_Id_Type) return Append_Partial_Result_Command_Type
            is
               C : Append_Partial_Result_Command_Type (State_Id);
            begin
               C.Handle   := Handle;
               C.Initialize (Work_Package_Id,
                             Task_Responsible_For_Result);
               return C;
            end Make;

            procedure Execute (This   : Append_Partial_Result_Command_Type;
                               Queues : Tasking.Queues_Access_Type;
                               Entity : in out Object.Entity'Class;
                               Report_Command_Successfully_Executed : Tasking.Procedure_With_Zero_Parameters_Access_Type)
            is
               pragma Unreferenced (Queues);

               Result : Correct_Number_To_Is_Prime_Number_Array_Entity_Type renames Correct_Number_To_Is_Prime_Number_Array_Entity_Type (Entity);
            begin
               for I in This.Handle.Ptr.Number_To_Is_Prime_Number_Array'Range loop
                  Result.Number_To_Is_Prime_Number_Array (I) := This.Handle.Ptr.Number_To_Is_Prime_Number_Array (I);
               end loop;
               Report_Command_Successfully_Executed.all;
            end Execute;

            procedure Execute_When_Finished (This   : Append_Partial_Result_Command_Type;
                                             Queues : Tasking.Queues_Access_Type;
                                             Entity : in out Object.Entity'Class)
            is
               pragma Unreferenced (This);
               pragma Unreferenced (Queues);

               Result : Correct_Number_To_Is_Prime_Number_Array_Entity_Type renames Correct_Number_To_Is_Prime_Number_Array_Entity_Type (Entity);
            begin
               End_Time_Stamp_When_Multi_Tasking := Ada.Real_Time.Clock;
               if Correct_Number_To_Is_Prime_Number_Array_Handle.Ptr.Number_To_Is_Prime_Number_Array = Result.Number_To_Is_Prime_Number_Array then
--                    Ada.Text_IO.Put_Line ("Correct");
                  null;
               else
                  Ada.Text_IO.Put_Line ("Not equal");
               end if;
            end Execute_When_Finished;

         end Commands;

         procedure Initialize_Worker_Tasks is
            CPU_Number : System.Multiprocessors.CPU_Range := System.Multiprocessors.CPU'First;
         begin
            for I in Queue_Index_Type'Range loop
               Command_Executors (I).Initialize (Id     => I,
                                                 Queues => Queues'Unchecked_Access,
                                                 CPU_Number => CPU_Number);
               if CPU_Number < System.Multiprocessors.Number_Of_CPUs then
                  CPU_Number := CPU_Number + 1;
               else
                  CPU_Number := System.Multiprocessors.CPU'First;
               end if;
            end loop;

            for I in Tasking.Worker_Task_Id_Type'Range loop
               Worker_Tasks (I).Start (Command_Executors (I)'Unchecked_Access);
            end loop;
         end Initialize_Worker_Tasks;

         State_And_Work_Package_Id_Generator : Tasking.State_And_Work_Package_Id_Generator_Type := Tasking.Make (Task_Id => 1);

         Work_Package_Id_For_Determining_Prime_Numbers : constant Tasking.Model.Fields.Work_Package_Identifer_Type := State_And_Work_Package_Id_Generator.Generate_New_Work_Package_Id;

         State_Id_For_Determining_Prime_Numbers : constant Tasking.Model.Fields.State_Id_Type := State_And_Work_Package_Id_Generator.Generate_New_State_Id;

         State_Id_As_64_Bits_For_Determining_Prime_Numbers : constant Tasking.Model.Fields.State_Id_As_64_Bits_Type :=
           Tasking.Model.Fields.To_64_Bits (State_Id_For_Determining_Prime_Numbers);

         Task_Responsible_For_Error_Handling_And_End_Result : constant Tasking.Worker_Task_Id_Type := Tasking.Random_Queue;

         Intervals : Integer_Intervals.Interval_Collection_Type;

         Number_Of_Parts_To_Divide_Into : constant Positive := 10*Number_Of_Worker_Tasks;
      begin
         Initialize_Worker_Tasks;

         declare
            Command : Tasking.Containers.Command_Holder_Type (Tasking.Model.Responsible_For_Potential_Error_Handling);
         begin
            Command.Responsible_For_Work_Package_Id := Work_Package_Id_For_Determining_Prime_Numbers;
            Command.Work_Package_Description.Initialize ("Determining whether a number is prime or not for all numbers from 1 to" & Max_Prime_Number'Img);
            Queues (Task_Responsible_For_Error_Handling_And_End_Result).Enqueue (Command);
         end;

         declare
            Command : Tasking.Containers.Command_Holder_Type (Tasking.Model.Custom_Command_With_State_Initializer);

            C : constant Commands.State_Initializer_Type := Commands.Make (State_Id                    => State_Id_As_64_Bits_For_Determining_Prime_Numbers,
                                                                           Work_Package_Id             => Work_Package_Id_For_Determining_Prime_Numbers,
                                                                           Task_Responsible_For_Result => Task_Responsible_For_Error_Handling_And_End_Result);
         begin
            Command.Custom_Command_Initializer.Replace_Element (C);
            Queues (Task_Responsible_For_Error_Handling_And_End_Result).Enqueue (Command);
         end;

         Intervals := Integer_Intervals.Get_Intervals (First                          => 1,
                                                       Last                           => Max_Prime_Number,
                                                       Number_Of_Parts_To_Divide_Into => Number_Of_Parts_To_Divide_Into);

         for Interval of Intervals loop
            declare
               Command : Tasking.Containers.Command_Holder_Type (Tasking.Model.Custom_Command);
               C : constant Commands.Determine_Whether_Prime_Or_Not_Command_Type := Commands.Make (Interval                    => Interval,
                                                                                                   State_Id                    => State_Id_As_64_Bits_For_Determining_Prime_Numbers,
                                                                                                   Work_Package_Id             => Work_Package_Id_For_Determining_Prime_Numbers,
                                                                                                   Task_Responsible_For_Result => Task_Responsible_For_Error_Handling_And_End_Result);
               Some_Random_Queue : constant Queue_Index_Type := Tasking.Random_Queue;
            begin
               Command.Custom_Command.Replace_Element (C);
               Queues (Some_Random_Queue).Enqueue (Command);
            end;
         end loop;

         declare
            Command : Tasking.Containers.Command_Holder_Type (Tasking.Model.Inform_Responsible_All_Commands_Created);
         begin
            Command.All_Commands_Created_For_Work_Package := Work_Package_Id_For_Determining_Prime_Numbers;
            Command.Expected_Number_Of_Commands_To_Execute := Tasking.Model.Fields.Expected_Number_Of_Commands_To_Execute_Type (Number_Of_Parts_To_Divide_Into);
            Queues (Task_Responsible_For_Error_Handling_And_End_Result).Enqueue (Command);
         end;

         Tasking.Shutdown_Application (Queues'Unchecked_Access);

      end Internal_Determine_Prime_Numbers_Multi_Tasking;

   begin
      Internal_Determine_Prime_Numbers_Multi_Tasking;
   end Determine_Prime_Numbers_Multi_Tasking;

   procedure Calculate_Correct_Solution is
      Number_To_Is_Prime_Number_Array_Entity : constant Number_To_Is_Prime_Number_Array_Entity_Access_Type := new Number_To_Is_Prime_Number_Array_Entity_Type (1, Max_Prime_Number);
   begin
      Correct_Number_To_Is_Prime_Number_Array_Handle.Set (Number_To_Is_Prime_Number_Array_Entity);

      for I in Positive range 1..Max_Prime_Number loop
         Number_To_Is_Prime_Number_Array_Entity.Number_To_Is_Prime_Number_Array (I) := Natural_Prime_Numbers.Is_Prime (I);
      end loop;
   end Calculate_Correct_Solution;

   procedure Calculate_Correct_Solution_When_Multi_Tasking (Number_Of_Worker_Tasks : Positive) is
      Start_Time_Stamp_When_Multi_Tasking : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      procedure Print_Total_Time is
         use type Ada.Real_Time.Time;

         Total_Time : constant Duration := Ada.Real_Time.To_Duration (End_Time_Stamp_When_Multi_Tasking - Start_Time_Stamp_When_Multi_Tasking);
      begin
         Ada.Text_IO.Put_Line ("Time to determine the prime numbers between 1 and" & Max_Prime_Number'Img & " took" &
                                 Total_Time'Img & " seconds for" & Number_Of_Worker_Tasks'Img & " tasks.");
      end Print_Total_Time;

   begin
      Determine_Prime_Numbers_Multi_Tasking (Number_Of_Worker_Tasks);
      Print_Total_Time;
   end Calculate_Correct_Solution_When_Multi_Tasking;

   Number_Of_CPUs : constant System.Multiprocessors.CPU := System.Multiprocessors.Number_Of_CPUs;
begin
   Ada.Text_IO.Put_Line ("This computer has a CPU with" & Number_Of_CPUs'Img & " cores and can therefore run this number of tasks in parallell.");
   declare
      Time_To_Calculate_Correct_Solution : constant Duration := Measure_Time (Calculate_Correct_Solution'Access);
   begin
      Ada.Text_IO.Put_Line ("Time to determine the prime numbers between 1 and" & Max_Prime_Number'Img & " took" &
                              Time_To_Calculate_Correct_Solution'Img & " seconds for one task.");
   end;

   for I in Positive range Positive (2)..Positive (Number_Of_CPUs) loop
      Calculate_Correct_Solution_When_Multi_Tasking (I);
   end loop;
end Main;
