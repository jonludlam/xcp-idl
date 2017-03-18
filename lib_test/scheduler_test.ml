open OUnit

let test_delay () =
  let open Scheduler.Delay in
  let x = make () in
  let before = Unix.gettimeofday () in
  ignore(wait x 0.5);
  let after = Unix.gettimeofday () in
  let elapsed = after -. before in
  assert_bool "elapsed_time1" (elapsed < 0.6);
  assert_bool "elapsed_time2" (elapsed > 0.4)

let test_delay_cancel () =
  let open Scheduler.Delay in
  let x = make () in
  let before = Unix.gettimeofday () in
  let th = Thread.create (fun () -> wait x 0.5) () in
  signal x;
  Thread.join th;
  let after = Unix.gettimeofday () in
  let elapsed = after -. before in
  assert_bool "elapsed_time1" (elapsed < 0.4)

let test_one_shot () =
  let after = ref None in
  let before = Unix.gettimeofday () in
  let _ = Scheduler.one_shot (Scheduler.Delta 1) "test_one_shot"
      (fun () -> after := Some (Unix.gettimeofday ())) in
  Thread.delay 2.0;
  let success =
    match !after with
    | Some x ->
      let elapsed = x -. before in
      elapsed > 0.99 && elapsed < 2.01
    | None ->
      false
  in
  assert_bool "one_shot_success" success

let test_one_shot_abs () =
  let after = ref None in
  let before = Unix.gettimeofday () in
  let now = Scheduler.now () in
  let _ = Scheduler.one_shot (Scheduler.Absolute (Int64.add 1L now)) "test_one_shot"
      (fun () -> after := Some (Unix.gettimeofday ())) in
  Thread.delay 2.0;
  let success =
    match !after with
    | Some x ->
      let elapsed = x -. before in
      elapsed > 0.99 && elapsed < 2.01
    | None ->
      false
  in
  assert_bool "one_shot_success" success

let test_one_shot_failure () =
  let after = ref None in
  let before = Unix.gettimeofday () in
  let _ = Scheduler.one_shot (Scheduler.Delta 0) "test_one_shot"
      (fun () -> after := failwith "Error") in
  let _ = Scheduler.one_shot (Scheduler.Delta 1) "test_one_shot"
      (fun () -> after := Some (Unix.gettimeofday ())) in
  Thread.delay 2.0;
  let success =
    match !after with
    | Some x ->
      let elapsed = x -. before in
      elapsed > 0.99 && elapsed < 2.01
    | None ->
      false
  in
  assert_bool "one_shot_success" success

let test_one_shot_cancel () =
  let after = ref None in
  let x = Scheduler.one_shot (Scheduler.Delta 1) "test_one_shot_cancel" (fun () -> after := Some (Unix.gettimeofday ())) in
  Scheduler.cancel x;
  Thread.delay 2.0;
  let success =
    match !after with
    | Some _ -> false
    | None -> true
  in
  assert_bool "one_shot_cancelled" success

let test_dump () =
  let after = ref None in
  let before = Unix.gettimeofday () in
  let _ = Scheduler.one_shot (Scheduler.Delta 1) "test_dump"
      (fun () -> after := Some (Unix.gettimeofday ())) in
  let dump = Scheduler.Dump.make () in
  assert_bool "dump_contains_item" (List.exists (fun x -> x.Scheduler.Dump.thing = "test_dump") dump)

let _ = Scheduler.start ()

let tests =
  "scheduler" >:::
    [
      "Test Delay" >:: test_delay;
      "Test Delay cancellation" >:: test_delay_cancel;
      "Test One shot" >:: test_one_shot;
      "Test One shot absolute" >:: test_one_shot_abs;
      "Test One shot failure" >:: test_one_shot_failure;
      "Test One shot cancellation" >:: test_one_shot_cancel;
      "Test dump" >:: test_dump;
    ]
