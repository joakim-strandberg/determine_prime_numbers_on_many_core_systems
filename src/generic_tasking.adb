with GNAT.Source_Info;
with Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with System.Multiprocessors.Dispatching_Domains;
with Ada.Task_Identification;

package body Generic_Tasking is

   use type System.Multiprocessors.CPU_Range;

   package Generator_Type_Owner is new Ada.Numerics.Discrete_Random (Result_Subtype => Queue_Index_Type);

   Generator : Generator_Type_Owner.Generator;

   package body Model is

      package body Fields is

         procedure Initialize (This         : out Work_Package_Identifer_Type;
                               Task_Id      : Task_Id_Type;
                               Task_Counter : Task_Counter_Command_Type) is
         begin
            This.Task_Id      := Task_Id;
            This.Task_Counter := Task_Counter;
         end Initialize;

         function To_String (This : Work_Package_Identifer_Type) return String is
         begin
            return "Task_Id:" & This.Task_Id'Img & ", Task_Counter:" & This.Task_Counter'Img;
         end To_String;

         function Convert_To_64_Bits_Unchecked is new Unchecked_Conversion (Source => State_Id_Type,
                                                                            Target => State_Id_As_64_Bits_Type);

         function Convert_From_64_Bits_Unchecked is new Unchecked_Conversion (Source => State_Id_As_64_Bits_Type,
                                                                              Target => State_Id_Type);


         function To_64_Bits (This : State_Id_Type) return State_Id_As_64_Bits_Type is
         begin
            return Convert_To_64_Bits_Unchecked (This);
         end To_64_Bits;

         function From_64_Bits (This : State_Id_As_64_Bits_Type) return State_Id_Type is
         begin
            return Convert_From_64_Bits_Unchecked (This);
         end From_64_Bits;
      end Fields;

      procedure Initialize (This            : in out Command_Type;
                            Work_Package_Id : Fields.Work_Package_Identifer_Type;
                            Task_Responsible_For_Result : Worker_Task_Id_Type) is
      begin
         This.Work_Package_Id := Work_Package_Id;
         This.Task_Responsible_For_Result := Task_Responsible_For_Result;
      end Initialize;

      procedure Initialize (This            : in out Command_With_State_Type;
                            Work_Package_Id : Fields.Work_Package_Identifer_Type;
                            Task_Responsible_For_Result : Worker_Task_Id_Type) is
      begin
         This.Work_Package_Id := Work_Package_Id;
         This.Task_Responsible_For_Result := Task_Responsible_For_Result;
      end Initialize;

      procedure Initialize (This            : in out Command_With_State_Initializer_Type;
                            Work_Package_Id : Fields.Work_Package_Identifer_Type;
                            Task_Responsible_For_Result : Worker_Task_Id_Type) is
      begin
         This.Work_Package_Id := Work_Package_Id;
         This.Task_Responsible_For_Result := Task_Responsible_For_Result;
      end Initialize;
   end Model;

   package body Tasks is

      use type Ada.Containers.Count_Type;
      use type State_Id_To_Entity_Map_Type_Owner.Cursor;
      use type Work_Package_Id_To_Error_Handling_Map_Type_Owner.Cursor;
      use type Model.Fields.Number_Of_Commands_Executed_Type;

      use all type Model.Command_Id_Type;

      function Work_Package_Id_Hash (Id : Model.Fields.Work_Package_Identifer_Type) return Ada.Containers.Hash_Type is
      begin
         return 8*Ada.Containers.Hash_Type (Id.Task_Id) + Ada.Containers.Hash_Type (Id.Task_Counter);
      end Work_Package_Id_Hash;

      procedure Initialize (This       : out Command_Executor_Type;
                            Id         : Queue_Index_Type;
                            Queues     : Array_Of_Queues_Access_Type;
                            CPU_Number : System.Multiprocessors.CPU_Range) is
      begin
--         Aida.Text_IO.Synchronized_Text_IO.Put_Line (GNAT.Source_Info.Source_Location &", ");
         This.Id         := Id;
         This.Queues     := Queues;
         This.CPU_Number := CPU_Number;
      end Initialize;

      procedure Enter_Command_Processing_Loop (This : in out Command_Executor_Type) is
         Shall_Continue : Boolean := True;

         procedure Handle_Command_Successfully_Executed (Work_Package_Id : Model.Fields.Work_Package_Identifer_Type) is
            C : Work_Package_Id_To_Error_Handling_Map_Type_Owner.Cursor;
         begin
            C := This.Work_Package_Id_To_Error_Handling_Map.Find (Work_Package_Id);
            if C = Work_Package_Id_To_Error_Handling_Map_Type_Owner.No_Element then
               Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, Task" &
                                                             Work_Package_Id.Task_Id'Img &
                                       ", Counted value" & Work_Package_Id.Task_Counter'Img &
                                    ", this error is very hard to debug since the work package description has been removed or never been specified/created!");
            else
               This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Number_Of_Commands_Executed
                 := This.Work_Package_Id_To_Error_Handling_Map.Constant_Reference (C).Number_Of_Commands_Executed + 1;

               if This.Work_Package_Id_To_Error_Handling_Map.Constant_Reference (C).Expected_Number_Of_Commands_To_Execute.Exists then
                  if
                    Integer (This.Work_Package_Id_To_Error_Handling_Map.Constant_Reference (C).Number_Of_Commands_Executed) =
                      Integer (This.Work_Package_Id_To_Error_Handling_Map.Constant_Reference (C).Expected_Number_Of_Commands_To_Execute.Value)
                  then
                     This.Work_Package_Id_To_Error_Handling_Map.Delete (C);
                  end if;
               end if;
            end if;
         end Handle_Command_Successfully_Executed;

         procedure Command_Succesfully_Executed (Task_Responsible_For_Result : Worker_Task_Id_Type;
                                                 Work_Package_Id             : Model.Fields.Work_Package_Identifer_Type) is
         begin
            if This.Id = Task_Responsible_For_Result then
               Handle_Command_Successfully_Executed (Work_Package_Id);
            else
               declare
                  Command_Successfully_Executed : Containers.Command_Holder_Type (Model.Command_Successfully_Executed);
               begin
                  Command_Successfully_Executed.Work_Package_Id := Work_Package_Id;
                  This.Queues (Task_Responsible_For_Result).Enqueue (Command_Successfully_Executed);
               end;
            end if;
         end Command_Succesfully_Executed;

         procedure Send_Stop_Processing_Work_Package_To_All_Tasks (Failed_Work_Package_Id : Model.Fields.Work_Package_Identifer_Type) is
            Number_Of_Tasks : constant Natural := Natural (Queue_Index_Type'Last) - Natural (Queue_Index_Type'First);
            -- This is the number of tasks minus the current task executing this procedure.

            C : Work_Package_Id_To_Error_Handling_Map_Type_Owner.Cursor;
         begin
            C := This.Work_Package_Id_To_Error_Handling_Map.Find (Failed_Work_Package_Id);
            if C /= Work_Package_Id_To_Error_Handling_Map_Type_Owner.No_Element then
               declare
                  Work_Package_Error_Handling : constant Work_Package_Error_Handling_Type :=
                    This.Work_Package_Id_To_Error_Handling_Map.Reference (C);
               begin
                  if not Work_Package_Error_Handling.Failed_Work_Package_Variables.Has_Failed then
                     if Number_Of_Tasks = 0 then
                        null;
                     elsif Number_Of_Tasks > 0 then
                        for I in Queue_Index_Type'Range loop
                           if I /= This.Id then
                              declare
                                 Stop_Command : Containers.Command_Holder_Type (Model.Stop_Processing_Work_Package);
                              begin
                                 Stop_Command.Priority := Containers.High;
                                 Stop_Command.Work_Package_To_Stop_Processing := Failed_Work_Package_Id;
                                 Stop_Command.Stop_Processing_Requester := This.Id;
                                 This.Queues (I).Enqueue (Stop_Command);
                              end;
                           end if;
                        end loop;
                        This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables
                          := (Has_Failed                                     => True,
                              Number_Of_Stop_Commands_Sent                   => Number_Of_Stop_Commands_Sent_Type (Number_Of_Tasks),
                              Number_Of_Stop_Command_Respones_Received       => 0,
                              Number_Of_Final_Stop_Commands_Sent             => 0,
                              Number_Of_Final_Stop_Command_Respones_Received => 0);
                     else
                        Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, Current task" &
                                                This.Id'Img &
                                                ", failed work package id " & Failed_Work_Package_Id.To_String);
                     end if;

                  else
                     null;
                     -- Not an error. It means at least two different commands that generated
                     -- an exception while being executed.
                  end if;
               end;
            else
               Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, missing expected element, Current task" &
                                       This.Id'Img &
                                       ", failed work package id " & Failed_Work_Package_Id.To_String);
            end if;
         end Send_Stop_Processing_Work_Package_To_All_Tasks;

         procedure Handle_Now_You_Are_Responsible_To_Handle_Failed_Work_Package (Failed_Work_Package_Id : Model.Fields.Work_Package_Identifer_Type) is
         begin
            Send_Stop_Processing_Work_Package_To_All_Tasks (Failed_Work_Package_Id);
         end Handle_Now_You_Are_Responsible_To_Handle_Failed_Work_Package;

         procedure Handle_Exception_That_Has_Occurred (Exception_Name              : String;
                                                       Exception_Message           : String;
                                                       Work_Package_Id             : Model.Fields.Work_Package_Identifer_Type;
                                                       Task_Responsible_For_Result : Worker_Task_Id_Type) is
         begin
            declare
               C : Failed_Work_Package_Id_Collection_Type_Owner.Cursor;
               Is_Success : Boolean;
            begin
               Handle_Error_Message (Exception_Name & ": " & Exception_Message);

               This.Failed_Work_Package_Id_Collection.Insert (New_Item => Work_Package_Id,
                                                              Position => C,
                                                              Inserted => Is_Success);

               if not Is_Success then
                  Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, Task" &
                                          Work_Package_Id.Task_Id'Img &
                                          ", Counted value" & Work_Package_Id.Task_Counter'Img);
               end if;
            end;

            -- Assign responsible task the cleanup of the failed work package
            if Task_Responsible_For_Result = This.Id then
               Handle_Now_You_Are_Responsible_To_Handle_Failed_Work_Package (Work_Package_Id);
               -- Handle the case where the task itself is responsible!!!
            else
               declare
                  Command : Containers.Command_Holder_Type (Model.Now_You_Are_Responsible_To_Handle_Failed_Work_Package);
               begin
                  Command.Work_Package_Id := Work_Package_Id;
                  This.Queues (Task_Responsible_For_Result).Enqueue (Command);
               end;
            end if;
         end Handle_Exception_That_Has_Occurred;

         procedure Handle_Custom_Command (Custom_Command : Model.Command_Type'Class) is

            procedure Internal_Command_Succesfully_Executed is
            begin
               Command_Succesfully_Executed (Custom_Command.Task_Responsible_For_Result,
                                             Custom_Command.Work_Package_Id);
            end Internal_Command_Succesfully_Executed;

         begin
            if This.Failed_Work_Package_Id_Collection.Contains (Custom_Command.Work_Package_Id) then
               null; -- Command should not be executed since it is part of a work package
                     -- where an exception has been thrown.
            else
               Custom_Command.Execute (Queues_Access_Type (This.Queues),
                                       Internal_Command_Succesfully_Executed'Unrestricted_Access);
            end if;
         exception
            when Unknown_Exception : others =>
               Handle_Exception_That_Has_Occurred (Exception_Name              => Ada.Exceptions.Exception_Name (Unknown_Exception),
                                                   Exception_Message           => Ada.Exceptions.Exception_Message (Unknown_Exception),
                                                   Work_Package_Id             => Custom_Command.Work_Package_Id,
                                                   Task_Responsible_For_Result => Custom_Command.Task_Responsible_For_Result);
         end Handle_Custom_Command;

         procedure Handle_Custom_Command_Summary (Custom_Command_With_State : Model.Command_With_State_Type'Class) is
            C : State_Id_To_Entity_Map_Type_Owner.Cursor;
         begin
            C := This.State_Id_To_Entity_Map.Find (Custom_Command_With_State.State_Id);
            if C = State_Id_To_Entity_Map_Type_Owner.No_Element then
               Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, Task" &
                                                             Custom_Command_With_State.Work_Package_Id.Task_Id'Img &
                                                             ", Counted value" & Custom_Command_With_State.Work_Package_Id.Task_Counter'Img);
            else
               declare
                  procedure Internal_Command_Succesfully_Executed is
                     Work_Package_Id_Cursor : Work_Package_Id_To_Error_Handling_Map_Type_Owner.Cursor;
                  begin
                     Work_Package_Id_Cursor := This.Work_Package_Id_To_Error_Handling_Map.Find (Custom_Command_With_State.Work_Package_Id);
                     if Work_Package_Id_Cursor = Work_Package_Id_To_Error_Handling_Map_Type_Owner.No_Element then
                        Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, Task" &
                                                Custom_Command_With_State.Work_Package_Id.Task_Id'Img &
                                                ", Counted value" & Custom_Command_With_State.Work_Package_Id.Task_Counter'Img &
                                                ", this error is very hard to debug since the work package description has been removed or never been specified/created!");
                     else
                        This.Work_Package_Id_To_Error_Handling_Map.Reference (Work_Package_Id_Cursor).Number_Of_Commands_Executed
                          := This.Work_Package_Id_To_Error_Handling_Map.Constant_Reference (Work_Package_Id_Cursor).Number_Of_Commands_Executed + 1;

                        if This.Work_Package_Id_To_Error_Handling_Map.Constant_Reference (Work_Package_Id_Cursor).Expected_Number_Of_Commands_To_Execute.Exists then
                           if
                             Integer (This.Work_Package_Id_To_Error_Handling_Map.Constant_Reference (Work_Package_Id_Cursor).Number_Of_Commands_Executed) =
                               Integer (This.Work_Package_Id_To_Error_Handling_Map.Constant_Reference (Work_Package_Id_Cursor).Expected_Number_Of_Commands_To_Execute.Value)
                           then
                              begin
                                 Custom_Command_With_State.Execute_When_Finished (Queues_Access_Type (This.Queues),
                                                                                  State_Id_To_Entity_Map_Type_Owner.Element (C).Ptr.all);
                              exception
                                 when Unknown_Exception : others =>
                                    Handle_Error_Message (Ada.Exceptions.Exception_Name (Unknown_Exception) & ": " &
                                                            Ada.Exceptions.Exception_Message (Unknown_Exception));
                              end;

                              This.Work_Package_Id_To_Error_Handling_Map.Delete (Work_Package_Id_Cursor);
                              This.State_Id_To_Entity_Map.Delete (Custom_Command_With_State.State_Id);
                           end if;
                        end if;
                     end if;
                  end Internal_Command_Succesfully_Executed;
               begin
                  Custom_Command_With_State.Execute (Queues_Access_Type (This.Queues),
                                                     State_Id_To_Entity_Map_Type_Owner.Element (C).Ptr.all,
                                                     Internal_Command_Succesfully_Executed'Unrestricted_Access);
               end;
            end if;
         exception
            when Unknown_Exception : others =>
               Handle_Exception_That_Has_Occurred (Exception_Name              => Ada.Exceptions.Exception_Name (Unknown_Exception),
                                                   Exception_Message           => Ada.Exceptions.Exception_Message (Unknown_Exception),
                                                   Work_Package_Id             => Custom_Command_With_State.Work_Package_Id,
                                                   Task_Responsible_For_Result => Custom_Command_With_State.Task_Responsible_For_Result);
         end Handle_Custom_Command_Summary;

         procedure Handle_Custom_Command_Initializer (Custom_Command_Initializer : Model.Command_With_State_Initializer_Type'Class) is
            C : State_Id_To_Entity_Map_Type_Owner.Cursor;
            Is_Success : Boolean;
         begin
            This.State_Id_To_Entity_Map.Insert (Key      => Custom_Command_Initializer.State_Id,
                                                New_Item => Custom_Command_Initializer.Entity_Handle,
                                                Position => C,
                                                Inserted => Is_Success);

            if not Is_Success then
               declare
                  W : Model.Fields.Work_Package_Identifer_Type renames Custom_Command_Initializer.Work_Package_Id;
               begin
                  Handle_Error_Message (GNAT.Source_Info.Source_Location &
                                                                ", Work package id already existed. Task" & W.Task_Id'Img &
                                                                " and task counter" & W.Task_Counter'Img);
--                      Aida.Text_IO.Synchronized_Text_IO.Put_Line (GNAT.Source_Info.Source_Location &
--                                                                  ", Work package id already existed. Task" & W.Task_Id'Img &
--                                                                  " and task counter" & W.Task_Counter'Img);
               end;
            end if;
         exception
            when Unknown_Exception : others =>
               Handle_Exception_That_Has_Occurred (Exception_Name              => Ada.Exceptions.Exception_Name (Unknown_Exception),
                                                   Exception_Message           => Ada.Exceptions.Exception_Message (Unknown_Exception),
                                                   Work_Package_Id             => Custom_Command_Initializer.Work_Package_Id,
                                                   Task_Responsible_For_Result => Custom_Command_Initializer.Task_Responsible_For_Result);
         end Handle_Custom_Command_Initializer;

         procedure Send_Stop_Command_To_All_Tasks is
         begin
            This.Tasks_Sent_Stop_Command_To.Clear;
            This.Tasks_Received_Stop_Command_Response_From.Clear;
            This.All_Tasks_Has_Responed_Empty_Queue := True;
            for I in Queue_Index_Type'Range loop
               if I /= This.Id then
                  declare
                     Stop_Command : Containers.Command_Holder_Type (Model.Stop_Command);
                  begin
                     Stop_Command.Requester := This.Id;
                     This.Queues (I).Enqueue (Stop_Command);
                     This.Tasks_Sent_Stop_Command_To.Include (I);
--                       Handle_Error_Message (GNAT.Source_Info.Source_Location & ", sent stop to task" & I'Img);
                  end;
               else
--                    Handle_Error_Message (GNAT.Source_Info.Source_Location & ", has not sent stop command to self (Task Id" & This.Id'Img & ")");
                  null;
               end if;
            end loop;

            if Queue_Index_Type'First = Queue_Index_Type'Last then
               Shall_Continue := False;
            end if;
         end Send_Stop_Command_To_All_Tasks;

         procedure Handle_Now_You_Are_Responsible_To_Stop_Application is
         begin
--              Handle_Error_Message (GNAT.Source_Info.Source_Location & ", will send stop to all tasks");
            Send_Stop_Command_To_All_Tasks;
         end Handle_Now_You_Are_Responsible_To_Stop_Application;

         procedure Handle_Stop_Command (Requester : Queue_Index_Type) is
         begin
            if This.Queues (This.Id).Current_Use = 0 then
               declare
                  Response_Command : Containers.Command_Holder_Type (Model.Stop_Response_When_Queue_Was_Empty);
               begin
                  Response_Command.Empty_Respondent := This.Id;
                  This.Queues (Requester).Enqueue (Response_Command);
--                    Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Task Id" & This.Id'Img & ", worker task queue was empty. Sent response.");
               end;
            else
               declare
                  Response_Command : Containers.Command_Holder_Type (Model.Stop_Response_When_Queue_Not_Empty);
               begin
                  Response_Command.Full_Respondent := This.Id;
                  This.Queues (Requester).Enqueue (Response_Command);
--                    Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Task Id" & This.Id'Img & ", worker task queue was not empty. Sent response.");
               end;
            end if;
         end Handle_Stop_Command;

         procedure Handle_Stop_Response_When_Queue_Was_Empty (Respondent : Queue_Index_Type) is
         begin
            This.Tasks_Received_Stop_Command_Response_From.Include (Respondent);
--              Handle_Error_Message (GNAT.Source_Info.Source_Location & ", received stop response from task" & Respondent'Img & ", number of received responses" &
--                                      This.Tasks_Received_Stop_Command_Response_From.Length'Img & ", expected" & This.Tasks_Sent_Stop_Command_To.Length'Img);
            if This.Tasks_Received_Stop_Command_Response_From.Length = This.Tasks_Sent_Stop_Command_To.Length then
               if This.All_Tasks_Has_Responed_Empty_Queue then
                  if This.Queues (This.Id).Current_Use = 0 then
                     for I in Queue_Index_Type'Range loop
                        if I /= This.Id then
                           declare
                              Stop_Command : Containers.Command_Holder_Type (Model.Stop_Immediately);
                           begin
                              This.Queues (I).Enqueue (Stop_Command);
                           end;
                        end if;
                     end loop;
                     Shall_Continue := False;
--                       Handle_Error_Message (GNAT.Source_Info.Source_Location & ", all worker task queues empty. Has sent stop immediately to all worker tasks and will stop execution of current worker task");
                  else
                     delay 0.01;
--                       Handle_Error_Message (GNAT.Source_Info.Source_Location & ", all worker task queues were not empty. Will send stop again");
                     Send_Stop_Command_To_All_Tasks;
                  end if;
               else
--                    Handle_Error_Message (GNAT.Source_Info.Source_Location & ", all worker task queues were not empty (current worker task queue not empty). Will send stop again");
                  Send_Stop_Command_To_All_Tasks;
               end if;
            end if;
         end Handle_Stop_Response_When_Queue_Was_Empty;

         procedure Handle_Stop_Response_When_Queue_Not_Empty (Respondent : Queue_Index_Type) is
         begin
            This.All_Tasks_Has_Responed_Empty_Queue := False;
            This.Tasks_Received_Stop_Command_Response_From.Include (Respondent);
--              Handle_Error_Message (GNAT.Source_Info.Source_Location & ", received stop response from task" & Respondent'Img & ", number of received responses" &
--                                      This.Tasks_Received_Stop_Command_Response_From.Length'Img & ", expected" & This.Tasks_Sent_Stop_Command_To.Length'Img);
            if This.Tasks_Received_Stop_Command_Response_From.Length = This.Tasks_Sent_Stop_Command_To.Length then
               delay 0.01;
--                 Handle_Error_Message (GNAT.Source_Info.Source_Location & ", will send stop again");
               Send_Stop_Command_To_All_Tasks;
            end if;
         end Handle_Stop_Response_When_Queue_Not_Empty;

         procedure Handle_Stop_Immediately is
         begin
            Shall_Continue := False;
         end Handle_Stop_Immediately;

         procedure Handle_Responsible_For_Potential_Error_Handling (Work_Package_Id          : Model.Fields.Work_Package_Identifer_Type;
                                                                    Work_Package_Description : Aida.Strings.Unbounded_String_Type)
         is
            C : Work_Package_Id_To_Error_Handling_Map_Type_Owner.Cursor;
            Is_Success : Boolean;

            New_Item : Work_Package_Error_Handling_Type;
         begin
            New_Item.Number_Of_Commands_Executed := 0;
            New_Item.Expected_Number_Of_Commands_To_Execute := (Exists => False);
            New_Item.Work_Package_Description := Work_Package_Description;

            This.Work_Package_Id_To_Error_Handling_Map.Insert (Key      => Work_Package_Id,
                                                               New_Item => New_Item,
                                                               Position => C,
                                                               Inserted => Is_Success);

            if not Is_Success then
               Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, Current task" &
                                                             This.Id'Img &
                                       ", failed work package id " & Work_Package_Id.To_String &
                                       ", and failed to: " & Work_Package_Description.To_String);
            end if;
         end Handle_Responsible_For_Potential_Error_Handling;

         procedure Handle_Inform_Responsible_All_Commands_Created (Work_Package_Id                        : Model.Fields.Work_Package_Identifer_Type;
                                                                   Expected_Number_Of_Commands_To_Execute : Model.Fields.Expected_Number_Of_Commands_To_Execute_Type)
         is
            C : Work_Package_Id_To_Error_Handling_Map_Type_Owner.Cursor;
         begin
            C := This.Work_Package_Id_To_Error_Handling_Map.Find (Work_Package_Id);

            if C /= Work_Package_Id_To_Error_Handling_Map_Type_Owner.No_Element then
               This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Expected_Number_Of_Commands_To_Execute :=
                 (Exists => True, Value => Expected_Number_Of_Commands_To_Execute);
            else
               Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, Current task" &
                                                             This.Id'Img &
                                       ", failed work package id " & Work_Package_Id.To_String);
            end if;
         end Handle_Inform_Responsible_All_Commands_Created;

         procedure Handle_Stop_Processing_Work_Package (Failed_Work_Package_Id    : Model.Fields.Work_Package_Identifer_Type;
                                                        Stop_Processing_Requester : Queue_Index_Type) is
         begin
            This.Failed_Work_Package_Id_Collection.Include (Failed_Work_Package_Id);
            -- The collection may already include the work package identifier
            -- because some command executed on this task may already have generated an exception

            declare
               Command : Containers.Command_Holder_Type (Model.Response_To_Stop_Processing_Work_Package);
            begin
               Command.Work_Package_Id := Failed_Work_Package_Id;
               This.Queues (Stop_Processing_Requester).Enqueue (Command);
            end;
         end Handle_Stop_Processing_Work_Package;

         procedure Handle_Response_To_Stop_Processing_Work_Package (Failed_Work_Package_Id : Model.Fields.Work_Package_Identifer_Type) is
            Number_Of_Tasks : constant Natural := Natural (Queue_Index_Type'Last) - Natural (Queue_Index_Type'First);
            -- This is the number of tasks minus the current task executing this procedure.

            C : Work_Package_Id_To_Error_Handling_Map_Type_Owner.Cursor;
         begin
            C := This.Work_Package_Id_To_Error_Handling_Map.Find (Failed_Work_Package_Id);
            if C /= Work_Package_Id_To_Error_Handling_Map_Type_Owner.No_Element then
               if This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Has_Failed then
                  This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Stop_Command_Respones_Received :=
                    This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Stop_Command_Respones_Received + 1;

                  if
                    Integer (This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Stop_Commands_Sent) =
                    Integer (This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Stop_Command_Respones_Received)
                  then
                     if Number_Of_Tasks = 0 then
                        null;
                     elsif Number_Of_Tasks > 0 then
                        for I in Queue_Index_Type'Range loop
                           if I /= This.Id then
                              declare
                                 Command : Containers.Command_Holder_Type (Model.Final_Stop_Processing_Work_Package);
                              begin
                                 Command.Work_Package_To_Stop_Processing := Failed_Work_Package_Id;
                                 Command.Stop_Processing_Requester := This.Id;
                                 This.Queues (I).Enqueue (Command);
                              end;
                           end if;
                        end loop;
                        This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Final_Stop_Commands_Sent
                          := Number_Of_Final_Stop_Commands_Sent_Type (Number_Of_Tasks);
                        This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Final_Stop_Command_Respones_Received
                          := 0;
                     else
                        Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, Current task" &
                                                This.Id'Img &
                                                ", failed work package id " & Failed_Work_Package_Id.To_String);
                     end if;
                  end if;
               else
                  Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, work package has not failed, Current task" &
                                          This.Id'Img &
                                          ", failed work package id " & Failed_Work_Package_Id.To_String);
               end if;
            else
               Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, missing expected element, Current task" &
                                       This.Id'Img &
                                       ", failed work package id " & Failed_Work_Package_Id.To_String);
            end if;
         end Handle_Response_To_Stop_Processing_Work_Package;

         procedure Handle_Final_Stop_Processing_Work_Package (Failed_Work_Package_Id    : Model.Fields.Work_Package_Identifer_Type;
                                                              Stop_Processing_Requester : Queue_Index_Type) is
         begin
            This.Failed_Work_Package_Id_Collection.Delete (Failed_Work_Package_Id);

            declare
               Command : Containers.Command_Holder_Type (Model.Response_To_Final_Stop_Processing_Work_Package);
            begin
               Command.Work_Package_Id := Failed_Work_Package_Id;
               This.Queues (Stop_Processing_Requester).Enqueue (Command);
            end;
         end Handle_Final_Stop_Processing_Work_Package;

         procedure Handle_Response_To_Final_Stop_Processing_Work_Package (Failed_Work_Package_Id : Model.Fields.Work_Package_Identifer_Type) is
            C : Work_Package_Id_To_Error_Handling_Map_Type_Owner.Cursor;
         begin
            C := This.Work_Package_Id_To_Error_Handling_Map.Find (Failed_Work_Package_Id);
            if C /= Work_Package_Id_To_Error_Handling_Map_Type_Owner.No_Element then
               if This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Has_Failed then
                  This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Final_Stop_Command_Respones_Received :=
                    This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Final_Stop_Command_Respones_Received + 1;

                  if
                    Integer (This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Final_Stop_Commands_Sent) =
                    Integer (This.Work_Package_Id_To_Error_Handling_Map.Reference (C).Failed_Work_Package_Variables.Number_Of_Final_Stop_Command_Respones_Received)
                  then
                     This.Work_Package_Id_To_Error_Handling_Map.Delete (C);
                     Handle_Error_Message (GNAT.Source_Info.Source_Location & ", no error, Current task" &
                                             This.Id'Img &
                                             ", failed work package id" & Failed_Work_Package_Id.To_String);
                  end if;
               else
                  Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, work package has not failed, Current task" &
                                          This.Id'Img &
                                          ", failed work package id " & Failed_Work_Package_Id.To_String);
               end if;
            else
               Handle_Error_Message (GNAT.Source_Info.Source_Location & ", Error, missing expected element, Current task" &
                                       This.Id'Img &
                                       ", failed work package id " & Failed_Work_Package_Id.To_String);
            end if;
         end Handle_Response_To_Final_Stop_Processing_Work_Package;

         procedure Remove_One_Command_From_The_Queue_And_Execute_It is
            Command : Containers.Command_Holder_Type;
         begin
            This.Queues (This.Id).Dequeue (Element => Command);
--                     Handle_Error_Message ("Task id" & This.Id'Img & ", " & Command.Id'Img);
            case Command.Id is
               when Custom_Command                                        => Handle_Custom_Command (Command.Custom_Command.Element);
               when Custom_Command_With_State                             => Handle_Custom_Command_Summary (Command.Custom_Command_With_State.Element);
               when Custom_Command_With_State_Initializer                 => Handle_Custom_Command_Initializer (Command.Custom_Command_Initializer.Element);
               when Now_You_Are_Responsible_To_Stop_Application           => Handle_Now_You_Are_Responsible_To_Stop_Application;
               when Stop_Command                                          => Handle_Stop_Command (Command.Requester);
               when Stop_Response_When_Queue_Was_Empty                    => Handle_Stop_Response_When_Queue_Was_Empty (Command.Empty_Respondent);
               when Stop_Response_When_Queue_Not_Empty                    => Handle_Stop_Response_When_Queue_Not_Empty (Command.Full_Respondent);
               when Stop_Immediately                                      => Handle_Stop_Immediately;
               when Now_You_Are_Responsible_To_Handle_Failed_Work_Package => Handle_Now_You_Are_Responsible_To_Handle_Failed_Work_Package (Command.Work_Package_Id);
               when Responsible_For_Potential_Error_Handling              => Handle_Responsible_For_Potential_Error_Handling (Command.Responsible_For_Work_Package_Id,
                                                                                                                              Command.Work_Package_Description);
               when Command_Successfully_Executed                         => Handle_Command_Successfully_Executed (Command.Work_Package_Id);
               when Inform_Responsible_All_Commands_Created               => Handle_Inform_Responsible_All_Commands_Created (Command.All_Commands_Created_For_Work_Package,
                                                                                                                             Command.Expected_Number_Of_Commands_To_Execute);
               when Stop_Processing_Work_Package                          => Handle_Stop_Processing_Work_Package (Command.Work_Package_To_Stop_Processing,
                                                                                                                  Command.Stop_Processing_Requester);
               when Response_To_Stop_Processing_Work_Package              => Handle_Response_To_Stop_Processing_Work_Package (Command.Work_Package_Id);
               when Final_Stop_Processing_Work_Package                    => Handle_Final_Stop_Processing_Work_Package (Command.Work_Package_To_Stop_Processing,
                                                                                                                        Command.Stop_Processing_Requester);
               when Response_To_Final_Stop_Processing_Work_Package        => Handle_Response_To_Final_Stop_Processing_Work_Package (Command.Work_Package_Id);
            end case;
         exception
            when Some_Exception : others =>
               Handle_Error_Message ("Task id" & This.Id'Img & ", " & Command.Id'Img &
                                       ", " & Ada.Exceptions.Exception_Name (Some_Exception) &
                                       ", " & Ada.Exceptions.Exception_Information (Some_Exception));
         end Remove_One_Command_From_The_Queue_And_Execute_It;
      begin
         if This.CPU_Number /= System.Multiprocessors.Not_A_Specific_CPU then
            System.Multiprocessors.Dispatching_Domains.Set_CPU (CPU => This.CPU_Number,
                                                                T   => Ada.Task_Identification.Current_Task);
         end if;
         while Shall_Continue loop
            begin
--                   Handle_Error_Message (GNAT.Source_Info.Source_Location &", Task Id" & This.Id'Img & ", Commands in queue" & This.Queues (This.Id).Current_Use'Img);
               if This.Queues (This.Id).Current_Use > 0 then
                  Remove_One_Command_From_The_Queue_And_Execute_It;
               else
                  delay 0.001;
               end if;
            exception
               when Unknown_Exception : others =>
                  Handle_Error_Message ("Task id" & This.Id'Img & ", " &
                                          ", " & Ada.Exceptions.Exception_Name (Unknown_Exception) &
                                        ", " & Ada.Exceptions.Exception_Information (Unknown_Exception));
            end;
         end loop;

         if Worker_Task_Id_Type'First = Worker_Task_Id_Type'Last then
            while This.Queues (This.Id).Current_Use > 0 loop
               Remove_One_Command_From_The_Queue_And_Execute_It;
            end loop;
         end if;

         This.State_Id_To_Entity_Map.Clear;
--         Aida.Text_IO.Synchronized_Text_IO.Put_Line ("Finished task" & This.Id'Img);
      end Enter_Command_Processing_Loop;

      task body Worker_Task is
         The_Command_Executor : Command_Executor_Access_Type;
      begin
         accept Start (Command_Executor : Command_Executor_Access_Type)
         do
            The_Command_Executor := Command_Executor;
         end Start;

         The_Command_Executor.Enter_Command_Processing_Loop;
      end Worker_Task;
   end Tasks;

   function Random_Queue return Queue_Index_Type is
   begin
      return Generator_Type_Owner.Random (Generator);
   end Random_Queue;

   procedure Shutdown_Application (Queues : Queues_Access_Type) is
      Random_Number : constant Queue_Index_Type := Generator_Type_Owner.Random (Generator);

      Responsability_Stop_Command : Containers.Command_Holder_Type (Model.Now_You_Are_Responsible_To_Stop_Application);
   begin
      --      Ada.Text_IO.Put_Line ("Random number:" & Random_Number'Img);
      Queues (Random_Number).Enqueue (Responsability_Stop_Command);
   end Shutdown_Application;

   function Make (Task_Id : Task_Id_Type) return State_And_Work_Package_Id_Generator_Type is
   begin
      return Work_Package_Generator : State_And_Work_Package_Id_Generator_Type do
         Work_Package_Generator.Task_Id := Task_Id;
      end return;
   end Make;

   function Generate_New_Work_Package_Id (This : in out State_And_Work_Package_Id_Generator_Type) return Model.Fields.Work_Package_Identifer_Type is
      Work_Package : Model.Fields.Work_Package_Identifer_Type;

      use type Model.Fields.Task_Counter_Command_Type;
   begin
      This.Task_Counter := This.Task_Counter + 1;
      Work_Package.Initialize (Task_Id      => This.Task_Id,
                               Task_Counter => This.Task_Counter);
      return Work_Package;
   end Generate_New_Work_Package_Id;

   function Generate_New_State_Id (This : in out State_And_Work_Package_Id_Generator_Type) return Model.Fields.State_Id_Type is
      State_Id : Model.Fields.State_Id_Type (This.Task_Id,
                                             This.State_Counter);
   begin
      This.State_Counter := This.State_Counter + 1;
      return State_Id;
   end Generate_New_State_Id;

end Generic_Tasking;
