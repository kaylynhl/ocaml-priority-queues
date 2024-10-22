open OUnit2
open A4.PriorityQueues

(* Test case modules *)
module LengthPrioritizedString = struct
  include String

  (** The priority of a string is its length. Example: [priority "hello" = 5] *)
  let priority s = length s
end

module StringPQ = MakeListPQ (LengthPrioritizedString)

(* Test case helpers *)

let test_is_empty_true =
  "Testing is_empty" >:: fun _ ->
  let q = StringPQ.empty in
  assert_equal true (StringPQ.is_empty q) ~printer:string_of_bool

let test_enqueue_blackbox =
  "Blackbox testing enqueue" >:: fun _ ->
  let q = StringPQ.enqueue "test" StringPQ.empty in
  assert_equal false (StringPQ.is_empty q) ~printer:string_of_bool

let test_enqueue_same_priorities =
  "Testing enqueue" >:: fun _ ->
  let q = StringPQ.enqueue "test" StringPQ.empty in
  let q = StringPQ.enqueue "work" q in
  let q = StringPQ.enqueue "satoru" q in
  let q = StringPQ.enqueue "reigen" q in
  assert_equal
    [ "test"; "work"; "satoru"; "reigen" ]
    (StringPQ.to_list q) ~printer:(String.concat "; ")

let test_enqueue =
  "Testing enqueue" >:: fun _ ->
  let q = StringPQ.enqueue "test" StringPQ.empty in
  assert_equal [ "test" ] (StringPQ.to_list q) ~printer:(String.concat "; ")

let test_front_empty =
  "Testing front with empty PQ raises error" >:: fun _ ->
  let q = StringPQ.empty in
  assert_raises StringPQ.Empty (fun _ -> StringPQ.front q)

let test_front =
  "Testing front" >:: fun _ ->
  let q = StringPQ.enqueue "test" StringPQ.empty in
  let q = StringPQ.enqueue "works" q in
  let q = StringPQ.enqueue "work" q in
  assert_equal "test" (StringPQ.front q) ~printer:Fun.id

let test_dequeue_blackbox =
  "Blackbox testing dequeue" >:: fun _ ->
  let q = StringPQ.enqueue "test" StringPQ.empty in
  let q = StringPQ.dequeue q in
  assert_equal true (StringPQ.is_empty q) ~printer:string_of_bool

let test_dequeue_empty =
  "Testing dequeue with empty PQ raises error" >:: fun _ ->
  let q = StringPQ.empty in
  assert_raises StringPQ.Empty (fun _ -> StringPQ.dequeue q)

let test_dequeue =
  "Testing dequeue" >:: fun _ ->
  let q = StringPQ.enqueue "test" StringPQ.empty in
  let q = StringPQ.enqueue "test2" q in
  let q = StringPQ.dequeue q in
  assert_equal [ "test2" ] (StringPQ.to_list q) ~printer:(String.concat "; ")

let test_string_pq =
  "Testing sample test case StringPQ" >:: fun _ ->
  let q = StringPQ.enqueue "hello" StringPQ.empty in
  let q = StringPQ.enqueue "world" q in
  let q = StringPQ.enqueue "this" q in
  let q = StringPQ.enqueue "is" q in
  let q = StringPQ.enqueue "a" q in
  let q = StringPQ.enqueue "test" q in
  assert_equal
    [ "a"; "is"; "this"; "test"; "hello"; "world" ]
    (StringPQ.to_list q) ~printer:(String.concat "; ")

(* Test suite *)
let tests =
  "test suite"
  >::: [
         test_is_empty_true;
         test_enqueue_blackbox;
         test_enqueue_same_priorities;
         test_enqueue;
         test_front_empty;
         test_front;
         test_dequeue_blackbox;
         test_dequeue_empty;
         test_dequeue;
         test_string_pq;
       ]

let _ = run_test_tt_main tests
