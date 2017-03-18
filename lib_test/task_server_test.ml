open OUnit

module TestInterface = struct
  let service_name = "test_interface"

  exception Does_not_exist of (string * string)
  exception Cancelled of string

  module Task = struct
    type id = string
    type async_result = string [@@deriving rpc]
    type completion_t = {
      duration : float;
      result: async_result option;
    }
    type state =
      | Pending of float
      | Completed of completion_t
      | Failed of Rpc.t
  end

  module Exception = struct
    type exnty =
      | Internal_error of string
      | Does_not_exist of string * string
      | Cancelled of string
      | Unknown of string [@@deriving rpc]
  end

  exception Internal_error of string

  let exnty_of_exn = function
    | Internal_error s -> Exception.Internal_error s
    | Cancelled s -> Exception.Cancelled s
    | Does_not_exist (x,y) -> Exception.Does_not_exist (x,y)
    | e -> Exception.Unknown (Printexc.to_string e)
  let exn_of_exnty = function
    | Exception.Internal_error s -> Internal_error s
    | Exception.Does_not_exist (x,y) -> Does_not_exist (x,y)
    | Exception.Cancelled s -> Cancelled s
    | Exception.Unknown s -> Failure s
end

module T = Task_server.Task(TestInterface)

let test_add () =
  let t = T.empty () in
  let task = T.add t "dbg" (fun task -> Some "done") in
  let ts = T.list t in
  assert_bool "Task in task list" (List.mem task ts)

let test_destroy () =
  let t = T.empty () in
  let task = T.add t "dbg" (fun task -> Some "done") in
  T.destroy t task.T.id;
  let ts = T.list t in
  assert_bool "Task not in task list" (not (List.mem task ts))

let test_run () =
  let t = T.empty () in
  let start = Unix.gettimeofday () in
  Thread.delay 0.001;
  let task = T.add t "dbg" (fun task -> Thread.delay 0.001; Some "done") in
  T.run task;
  assert_bool "Task ctime" (task.T.ctime > start);
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Completed {TestInterface.Task.result=Some r; duration} ->
       r = "done" && duration > 0.0
     | _ -> false)

let test_raise () =
  Debug.disable "task_server";
  let t = T.empty () in
  let task = T.add t "dbg" (fun task -> raise (TestInterface.Internal_error "test")) in
  T.run task;
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let s = TestInterface.Exception.exnty_of_rpc r in
           s = TestInterface.Exception.Internal_error "test"
         with _ -> false
       end
     | _ -> false)

let test_cancel () =
  let t = T.empty () in
  let task = T.add t "dbg" (fun task -> T.check_cancelling task; Some "foo") in
  T.cancel t task.T.id;
  T.run task;
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled (task.T.id)
         with _ -> false end
     | _ -> false)

let test_with_cancel () =
  let t = T.empty () in
  let cancel_fn_run = ref false in
  let task = T.add t "dbg"
      (fun task -> T.with_cancel task (fun () -> cancel_fn_run := true) (fun () -> Some "foo")) in
  T.cancel t task.T.id;
  T.run task;
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled (task.T.id)
         with _ -> false end
     | _ -> false);
  assert_bool "Cancel_fn run" !cancel_fn_run

let test_with_cancel_failure () =
  let t = T.empty () in
  let task = T.add t "dbg"
      (fun task -> T.with_cancel task (fun () -> failwith "moo") (fun () -> Some "foo")) in
  T.cancel t task.T.id;
  T.run task;
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled (task.T.id)
         with _ -> false end
     | _ -> false)

let test_with_cancel2 () =
  let t = T.empty () in
  let delay = Scheduler.Delay.make () in
  let cancel_fn_run = ref false in
  let task = T.add t "dbg"
      (fun task ->
         T.with_cancel task (fun () -> cancel_fn_run := true)
           (fun () -> ignore (Scheduler.Delay.wait delay 1.0);
             T.check_cancelling task; Some "foo")) in
  let th = Thread.create (fun () -> T.run task) () in
  Thread.delay 0.01;
  T.cancel t task.T.id;
  Scheduler.Delay.signal delay;
  Thread.join th;
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled (task.T.id)
         with _ -> false end
     | _ -> false);
  assert_bool "Cancel_fn run" !cancel_fn_run

let test_with_cancel_failure2 () =
  let t = T.empty () in
  let delay = Scheduler.Delay.make () in
  let task = T.add t "dbg"
      (fun task ->
         T.with_cancel task (fun () -> failwith "moo")
           (fun () -> ignore (Scheduler.Delay.wait delay 1.0);
             T.check_cancelling task; Some "foo")) in
  let th = Thread.create (fun () -> T.run task) () in
  Thread.delay 0.01;
  assert_raises (TestInterface.Does_not_exist ("task","moo")) (fun () -> T.cancel t "moo");
  T.cancel t task.T.id;
  Scheduler.Delay.signal delay;
  Thread.join th;
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled (task.T.id)
         with _ -> false end
     | _ -> false)

let test_subtasks () =
  let t = T.empty () in
  let task = T.add t "dbg"
      (fun task ->
        let _ : int = T.with_subtask task "subtask1" (fun () -> 0) in
         Some "done") in
  T.run task;
  assert_bool "Subtasks"
    ((List.hd task.T.subtasks |> fst) = "subtask1");
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Completed {TestInterface.Task.result=Some r; duration} ->
       r = "done"
     | _ -> false)

let test_subtasks_failure () =
  let t = T.empty () in
  let task = T.add t "dbg"
      (fun task ->
         let _ : int = T.with_subtask task "subtask1"
             (fun () -> raise (TestInterface.Internal_error "foo")) in
         Some "done") in
  T.run task;
  let subtask = List.hd task.T.subtasks in
  assert_bool "Subtasks"
    (fst subtask = "subtask1");
  assert_bool "Subtasks"
    (match snd subtask with
     | TestInterface.Task.Failed r ->
       r |> TestInterface.Exception.exnty_of_rpc = TestInterface.Exception.Internal_error "foo"
     | _ -> false);
  assert_bool "Task result"
    (match task.T.state with
     | TestInterface.Task.Failed r ->
       r |> TestInterface.Exception.exnty_of_rpc = TestInterface.Exception.Internal_error "foo"
     | _ -> false)

let test_cancel_trigger () =
  let t = T.empty () in
  T.set_cancel_trigger t "dbg" 5;
  let xxx = ref 0 in
  let dbg = ref 0 in
  let fn x task =
    let rec loop n =
      x := n;
      if n=0 then () else (T.check_cancelling task; loop (n-1))
    in loop 10;
    Some "done"
  in
  let task1 = T.add t "xxx" (fn xxx) in
  let task2 = T.add t "dbg" (fn dbg) in
  T.run task1;
  T.run task2;
  assert_bool "Task result"
    (match task2.T.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled (task2.T.id)
         with _ -> false end
     | _ -> false);
  assert_bool "Task result"
    (match task1.T.state with
     | TestInterface.Task.Completed {TestInterface.Task.result=Some r; duration} ->
       r = "done"
      | _ -> false);
  assert_bool "cancel points xxx" (!xxx = 0);
  assert_bool "cancel points dbg" (!dbg = 6)

let tests =
  "Task_server tests" >:::
  [
    "Test adding a task" >:: test_add;
    "Test removing a task" >:: test_destroy;
    "Test run" >:: test_run;
    "Test raise" >:: test_raise;
    "Test cancel" >:: test_cancel;
    "Test with_cancel" >:: test_with_cancel;
    "Test with_cancel_failure" >:: test_with_cancel_failure;
    "Test with_cancel 2" >:: test_with_cancel2;
    "Test with_cancel_failure2" >:: test_with_cancel_failure2;
    "Test subtasks" >:: test_subtasks;
    "Test subtask failure" >:: test_subtasks_failure;
    "Test cancel trigger" >:: test_cancel_trigger;
  ]
