with Ada.Containers.Hashed_Sets;
with Interfaces;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Hashed_Maps;
with Aida.Strings;

with Object.Handle;
with System.Multiprocessors;

generic
   type Queue_Index_Type is range <>;
   type Task_Id_Type is range <>;
   Handle_Error_Message : access protected procedure (M : String);
package Generic_Tasking is
   pragma Elaborate_Body;

   subtype Worker_Task_Id_Type is Queue_Index_Type;

   package Entity_Handle is new Object.Handle (Object_Type     => Object.Entity'Class,
                                               Object_Type_Ptr => Object.Entity_Ptr);

   type Queues_Access_Type;

   type Procedure_With_Zero_Parameters_Access_Type is access procedure;

   package Model is

      package Fields is

         type Task_Counter_Command_Type is new Interfaces.Unsigned_32;

         type State_Counter_Type is new Interfaces.Unsigned_32;
         for State_Counter_Type'Size use 32;

         type Work_Package_Identifer_Type is tagged private;
         -- A work package consists of one "task" that should be done but that
         -- can be split into many independent chores. The chores will be executed
         -- on different tasks and when a chore is finished its results will be
         -- sent to the task responsible for gathering the results.

         procedure Initialize (This         : out Work_Package_Identifer_Type;
                               Task_Id      : Task_Id_Type;
                               Task_Counter : Task_Counter_Command_Type);

         function Task_Id (This : Work_Package_Identifer_Type) return Task_Id_Type;

         function Task_Counter (This : Work_Package_Identifer_Type) return Task_Counter_Command_Type;

         function To_String (This : Work_Package_Identifer_Type) return String;

         type State_Id_As_64_Bits_Type is new Interfaces.Integer_64;
         for State_Id_As_64_Bits_Type'Size use 64;

         type State_Id_Type (Task_Id       : Task_Id_Type;
                             State_Counter : State_Counter_Type) is null record;

         function To_64_Bits (This : State_Id_Type) return State_Id_As_64_Bits_Type;

         function From_64_Bits (This : State_Id_As_64_Bits_Type) return State_Id_Type;

         type Expected_Number_Of_Commands_To_Execute_Type is new Integer;

         type Number_Of_Commands_Executed_Type is new Integer;

      private

         type Work_Package_Identifer_Type is tagged
            record
               Task_Id      : Task_Id_Type;
               Task_Counter : Task_Counter_Command_Type;
            end record;

         function Task_Id (This : Work_Package_Identifer_Type) return Task_Id_Type is (This.Task_Id);

         function Task_Counter (This : Work_Package_Identifer_Type) return Task_Counter_Command_Type is (This.Task_Counter);

         function Task_Id (This : State_Id_Type) return Task_Id_Type is (This.Task_Id);

         function State_Counter (This : State_Id_Type) return Fields.State_Counter_Type is (This.State_Counter);

      end Fields;

      type Command_Id_Type is (
                               Custom_Command,
                               Custom_Command_With_State,
                               Custom_Command_With_State_Initializer,
                               -- Used for controlled shutdown of the application
                               Now_You_Are_Responsible_To_Stop_Application,
                               Stop_Command,
                               Stop_Response_When_Queue_Was_Empty,
                               Stop_Response_When_Queue_Not_Empty,
                               Stop_Immediately,
                               -- Used for graceful error handling
                               Responsible_For_Potential_Error_Handling,
                               Inform_Responsible_All_Commands_Created,
                               Command_Successfully_Executed,
                               Now_You_Are_Responsible_To_Handle_Failed_Work_Package,
                               Stop_Processing_Work_Package, -- Sent with high priority
                               Response_To_Stop_Processing_Work_Package,
                               Final_Stop_Processing_Work_Package,
                               Response_To_Final_Stop_Processing_Work_Package
                              );

      type Command_Type;
      type Command_With_State_Type;
      type Command_With_State_Initializer_Type;

      type Command_Type is abstract tagged private;
      -- How should one task instruct another task about which command it should
      -- execute? Imagine the information about what should be done is encoded in
      -- a command object. There are two ways in which it could be communicated
      -- to the other task.
      -- 1. The command object is located on the stack of the "commanding" task
      --    and the whole object is copied to the stack of the other task.
      -- 2. The command object is located on the heap and the "commanding" task
      --    has a pointer to the object located on the task stack. It is the pointer
      --    that is copied from the "commanding" task to the stack of the other task.
      -- The current implementation has chosen solution 1. This avoids using smart
      -- pointers (how else could one guarantee against memory leaks?) and
      -- thus avoiding usage of controlled types which are widely known to have
      -- a large performance cost. Ignoring the performance cost of controlled types,
      -- an educated guess is that there is no avoiding the copying of memory since
      -- the memory on the would still be needed to be mapped to the cache memory of
      -- the CPU. To know for sure one would need to make two implementations and
      -- then make performance measurements.

      procedure Initialize (This                        : in out Command_Type;
                            Work_Package_Id             : Fields.Work_Package_Identifer_Type;
                            Task_Responsible_For_Result : Worker_Task_Id_Type);

      function Work_Package_Id (This : Command_Type) return Fields.Work_Package_Identifer_Type;

      function Task_Responsible_For_Result (This : Command_Type) return Worker_Task_Id_Type;

      procedure Execute (This                                 : Command_Type;
                         Queues                               : Queues_Access_Type;
                         Report_Command_Successfully_Executed : Procedure_With_Zero_Parameters_Access_Type) is abstract;

      type Command_With_State_Type (State_Id : Fields.State_Id_As_64_Bits_Type) is abstract tagged private;

      procedure Initialize (This            : in out Command_With_State_Type;
                            Work_Package_Id : Fields.Work_Package_Identifer_Type;
                            Task_Responsible_For_Result : Worker_Task_Id_Type);

      procedure Execute (This   : Command_With_State_Type;
                         Queues : Queues_Access_Type;
                         Entity : in out Object.Entity'Class;
                         Report_Command_Successfully_Executed : Procedure_With_Zero_Parameters_Access_Type) is abstract;

      procedure Execute_When_Finished (This   : Command_With_State_Type;
                                       Queues : Queues_Access_Type;
                                       Entity : in out Object.Entity'Class) is abstract;

      function Work_Package_Id (This : Command_With_State_Type) return Fields.Work_Package_Identifer_Type;

      function Task_Responsible_For_Result (This : Command_With_State_Type) return Worker_Task_Id_Type;

      type Command_With_State_Initializer_Type (State_Id : Fields.State_Id_As_64_Bits_Type) is abstract tagged private;

      procedure Initialize (This            : in out Command_With_State_Initializer_Type;
                            Work_Package_Id : Fields.Work_Package_Identifer_Type;
                            Task_Responsible_For_Result : Worker_Task_Id_Type);

      function Entity_Handle (This : Command_With_State_Initializer_Type) return Entity_Handle.Handle is abstract;

      function Work_Package_Id (This : Command_With_State_Initializer_Type) return Fields.Work_Package_Identifer_Type;

      function Task_Responsible_For_Result (This : Command_With_State_Initializer_Type) return Worker_Task_Id_Type;

   private

      type Command_Type is abstract tagged
         record
            Work_Package_Id : Fields.Work_Package_Identifer_Type;
            Task_Responsible_For_Result : Worker_Task_Id_Type;
         end record;

      function Work_Package_Id (This : Command_Type) return Fields.Work_Package_Identifer_Type is (This.Work_Package_Id);

      function Task_Responsible_For_Result (This : Command_Type) return Worker_Task_Id_Type is (This.Task_Responsible_For_Result);

      type Command_With_State_Type (State_Id : Fields.State_Id_As_64_Bits_Type) is abstract tagged
         record
            Work_Package_Id : Fields.Work_Package_Identifer_Type;
            Task_Responsible_For_Result : Worker_Task_Id_Type;
         end record;

      function Work_Package_Id (This : Command_With_State_Type) return Fields.Work_Package_Identifer_Type is (This.Work_Package_Id);

      function Task_Responsible_For_Result (This : Command_With_State_Type) return Worker_Task_Id_Type is (This.Task_Responsible_For_Result);

      type Command_With_State_Initializer_Type (State_Id : Fields.State_Id_As_64_Bits_Type) is abstract tagged
         record
            Work_Package_Id : Fields.Work_Package_Identifer_Type;
            Task_Responsible_For_Result : Worker_Task_Id_Type;
         end record;

      function Work_Package_Id (This : Command_With_State_Initializer_Type) return Fields.Work_Package_Identifer_Type is (This.Work_Package_Id);

      function Task_Responsible_For_Result (This : Command_With_State_Initializer_Type) return Worker_Task_Id_Type is (This.Task_Responsible_For_Result);

   end Model;

   use type Model.Fields.State_Counter_Type;

   package Containers is

      use all type Model.Command_Id_Type;

      package Command_Holder is new Ada.Containers.Indefinite_Holders (Element_Type => Model.Command_Type'Class,
                                                                       "="          => Model."=");

      package Command_Summary_Holder is new Ada.Containers.Indefinite_Holders (Element_Type => Model.Command_With_State_Type'Class,
                                                                               "="          => Model."=");

      package Command_Summary_Initializer_Holder is new Ada.Containers.Indefinite_Holders (Element_Type => Model.Command_With_State_Initializer_Type'Class,
                                                                                           "="          => Model."=");
      type Command_Queue_Priority_Type is (
                                           Normal,
                                           High  -- Used to signal when work a chore in a work package has generated an exception
                                           );

      type Command_Holder_Type (Id : Model.Command_Id_Type := Model.Custom_Command) is
         record
            Priority : Command_Queue_Priority_Type := Normal;
            case Id is
               when Model.Custom_Command =>
                  Custom_Command : Command_Holder.Holder;
               when Model.Custom_Command_With_State =>
                  Custom_Command_With_State : Command_Summary_Holder.Holder;
               when Model.Custom_Command_With_State_Initializer =>
                  Custom_Command_Initializer : Command_Summary_Initializer_Holder.Holder;
               when Model.Now_You_Are_Responsible_To_Stop_Application =>
                  null;
               when Model.Stop_Immediately =>
                  null;
               when Model.Stop_Command =>
                  Requester : Queue_Index_Type;
               when Model.Stop_Response_When_Queue_Was_Empty =>
                  Empty_Respondent : Queue_Index_Type;
               when Model.Stop_Response_When_Queue_Not_Empty =>
                  Full_Respondent : Queue_Index_Type;
               when Model.Now_You_Are_Responsible_To_Handle_Failed_Work_Package |
                    Model.Command_Successfully_Executed |
                    Model.Response_To_Stop_Processing_Work_Package |
                    Model.Response_To_Final_Stop_Processing_Work_Package =>
                  Work_Package_Id : Model.Fields.Work_Package_Identifer_Type;
               when Model.Responsible_For_Potential_Error_Handling =>
                  Responsible_For_Work_Package_Id : Model.Fields.Work_Package_Identifer_Type;
                  Work_Package_Description        : Aida.Strings.Unbounded_String_Type;
               when Model.Inform_Responsible_All_Commands_Created =>
                  All_Commands_Created_For_Work_Package  : Model.Fields.Work_Package_Identifer_Type;
                  Expected_Number_Of_Commands_To_Execute : Model.Fields.Expected_Number_Of_Commands_To_Execute_Type;
               when Model.Stop_Processing_Work_Package |
                    Model.Final_Stop_Processing_Work_Package =>
                  Work_Package_To_Stop_Processing : Model.Fields.Work_Package_Identifer_Type;
                  Stop_Processing_Requester : Queue_Index_Type;
            end case;
         end record;

      package Synchronized_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Command_Holder_Type);

      function Get_Priority (Command : Command_Holder_Type) return Command_Queue_Priority_Type is (Command.Priority);

      function Before (L, R : Command_Queue_Priority_Type) return Boolean is (Command_Queue_Priority_Type'Pos (L) < Command_Queue_Priority_Type'Pos (R));

      package Command_Queue_Type_Owner is new Ada.Containers.Unbounded_Priority_Queues (Queue_Interfaces => Synchronized_Queue_Interface,
                                                                                        Queue_Priority   => Command_Queue_Priority_Type,
                                                                                        Get_Priority     => Get_Priority,
                                                                                        Before           => Before);

      type Queue_Access_Type is access all Command_Queue_Type_Owner.Queue;
   end Containers;

   package Tasks is

      type Array_Of_Queues_Type is array (Queue_Index_Type) of aliased Containers.Command_Queue_Type_Owner.Queue;

      type Array_Of_Queues_Access_Type is access all Array_Of_Queues_Type;

      type Command_Executor_Type is tagged limited private;

      procedure Initialize (This       : out Command_Executor_Type;
                            Id         : Queue_Index_Type;
                            Queues     : Array_Of_Queues_Access_Type;
                            CPU_Number : System.Multiprocessors.CPU_Range);

      procedure Enter_Command_Processing_Loop (This : in out Command_Executor_Type);

      type Command_Executor_Access_Type is access all Command_Executor_Type;

      task type Worker_Task is
         entry Start (Command_Executor : Command_Executor_Access_Type);
         -- Each worker task is given a unique identifier and it has a queue of commands
         -- to be executed. This entry subprogram was created instead of having
         -- this information be passed to the worker task through discriminants.
         -- This makes it much more simple to create an array of worker tasks
         -- and then initialize them after creation.
         --
         -- The queue object is assumed to be instantiated at library level,
         -- and therefore there is no need for smart pointers and memory management.
         -- Ordinary access type is enough.
      end Worker_Task;

   private

      use type Ada.Containers.Hash_Type;

      function Hash (Element : Queue_Index_Type) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type (Element));

      package Tasks_Sent_Stop_Command_To_Set_Type_Owner is new Ada.Containers.Hashed_Sets (Element_Type        => Queue_Index_Type,
                                                                                           Hash                => Hash,
                                                                                           Equivalent_Elements => "=",
                                                                                           "="                 => "=");

      function Hash (Element : Model.Fields.State_Id_As_64_Bits_Type) return Ada.Containers.Hash_Type is
        (Ada.Containers.Hash_Type (Model.Fields.State_Counter_Type (Model.Fields.From_64_Bits (Element).Task_Id)*
                                     Model.Fields.From_64_Bits (Element).State_Counter));

      package State_Id_To_Entity_Map_Type_Owner is new Ada.Containers.Hashed_Maps (Key_Type        => Model.Fields.State_Id_As_64_Bits_Type,
                                                                                   Element_Type    => Entity_Handle.Handle,
                                                                                   Hash            => Hash,
                                                                                   Equivalent_Keys => Model.Fields."=",
                                                                                   "="             => Entity_Handle."=");

      function Work_Package_Id_Hash (Id : Model.Fields.Work_Package_Identifer_Type) return Ada.Containers.Hash_Type;

      package Failed_Work_Package_Id_Collection_Type_Owner is new Ada.Containers.Hashed_Sets (Element_Type        => Model.Fields.Work_Package_Identifer_Type,
                                                                                              Hash                => Work_Package_Id_Hash,
                                                                                              Equivalent_Elements => Model.Fields."=",
                                                                                              "="                 => Model.Fields."=");

      function Hash_Of_Work_Package_Id (Element : Model.Fields.Work_Package_Identifer_Type) return Ada.Containers.Hash_Type is
        (2*Ada.Containers.Hash_Type (Element.Task_Id)+3*Ada.Containers.Hash_Type (Element.Task_Counter));

      package Work_Package_Id_To_Tasks_Type_Owner is new Ada.Containers.Hashed_Maps (Key_Type        => Model.Fields.Work_Package_Identifer_Type,
                                                                                     Element_Type    => Tasks_Sent_Stop_Command_To_Set_Type_Owner.Set,
                                                                                     Hash            => Hash_Of_Work_Package_Id,
                                                                                     Equivalent_Keys => Model.Fields."=",
                                                                                     "="             => Tasks_Sent_Stop_Command_To_Set_Type_Owner."=");

      type Nullable_Expected_Number_Of_Commands_To_Execute_Type (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Model.Fields.Expected_Number_Of_Commands_To_Execute_Type;
               when False => null;
            end case;
         end record;

      type Number_Of_Stop_Commands_Sent_Type is new Natural;

      type Number_Of_Stop_Command_Respones_Received_Type is new Natural;

      type Number_Of_Final_Stop_Commands_Sent_Type is new Natural;

      type Number_Of_Final_Stop_Command_Respones_Received_Type is new Natural;

      type Failed_Work_Package_Variables_Type (Has_Failed : Boolean := False) is
         record
            case Has_Failed is
               when True =>
                  Number_Of_Stop_Commands_Sent                   : Number_Of_Stop_Commands_Sent_Type;
                  Number_Of_Stop_Command_Respones_Received       : Number_Of_Stop_Command_Respones_Received_Type;
                  Number_Of_Final_Stop_Commands_Sent             : Number_Of_Final_Stop_Commands_Sent_Type;
                  Number_Of_Final_Stop_Command_Respones_Received : Number_Of_Final_Stop_Command_Respones_Received_Type;
               when False =>
                  null;
            end case;
         end record;

      type Work_Package_Error_Handling_Type is
         record
            Number_Of_Commands_Executed            : Model.Fields.Number_Of_Commands_Executed_Type := 0;
            Expected_Number_Of_Commands_To_Execute : Nullable_Expected_Number_Of_Commands_To_Execute_Type;
            Work_Package_Description               : Aida.Strings.Unbounded_String_Type;
            Failed_Work_Package_Variables          : Failed_Work_Package_Variables_Type;
         end record;

      package Work_Package_Id_To_Error_Handling_Map_Type_Owner is new Ada.Containers.Hashed_Maps (Key_Type        => Model.Fields.Work_Package_Identifer_Type,
                                                                                                  Element_Type    => Work_Package_Error_Handling_Type,
                                                                                                  Hash            => Hash_Of_Work_Package_Id,
                                                                                                  Equivalent_Keys => Model.Fields."=",
                                                                                                  "="             => "=");

      type Command_Executor_Type is tagged limited
         record
            Id                                                   : Queue_Index_Type;
            Queues                                               : Array_Of_Queues_Access_Type;
            CPU_Number                                           : System.Multiprocessors.CPU_Range;
            Tasks_Sent_Stop_Command_To                           : Tasks_Sent_Stop_Command_To_Set_Type_Owner.Set;
            Tasks_Received_Stop_Command_Response_From            : Tasks_Sent_Stop_Command_To_Set_Type_Owner.Set;
            All_Tasks_Has_Responed_Empty_Queue                   : Boolean := True;
            State_Id_To_Entity_Map                               : State_Id_To_Entity_Map_Type_Owner.Map;
            Failed_Work_Package_Id_Collection                    : Failed_Work_Package_Id_Collection_Type_Owner.Set;
            Work_Package_Id_To_Error_Handling_Map                : Work_Package_Id_To_Error_Handling_Map_Type_Owner.Map;
         end record;
   end Tasks;

   type Queues_Access_Type is access all Tasks.Array_Of_Queues_Type;

   procedure Shutdown_Application (Queues : Queues_Access_Type);

   function Random_Queue return Queue_Index_Type;

   type State_And_Work_Package_Id_Generator_Type (<>) is tagged limited private;

   function Make (Task_Id : Task_Id_Type) return State_And_Work_Package_Id_Generator_Type;

   function Generate_New_Work_Package_Id (This : in out State_And_Work_Package_Id_Generator_Type) return Model.Fields.Work_Package_Identifer_Type;

   function Generate_New_State_Id (This : in out State_And_Work_Package_Id_Generator_Type) return Model.Fields.State_Id_Type;

private

   type State_And_Work_Package_Id_Generator_Type is tagged limited
      record
         Task_Id      : Task_Id_Type;
         Task_Counter : Model.Fields.Task_Counter_Command_Type := 0;
         State_Counter : Model.Fields.State_Counter_Type := 0;
      end record;

end Generic_Tasking;
