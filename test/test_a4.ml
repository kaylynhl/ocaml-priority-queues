open OUnit2
open A4.PriorityQueues
include A4.Patient

(* Test case helpers *)
module LengthPrioritizedString = struct
  include String

  (** The priority of a string is its length. Example: [priority "hello" = 5] *)
  let priority s = length s
end

module StringPQ = MakeListPQ (LengthPrioritizedString)
module PatientListPQ = MakeListPQ (A4.Patient)

(** [enqueue_elements elements] enqueues each element of the list of elements
    [elements] to an empty StringPQ. *)
let enqueue_elements elements =
  List.fold_left
    (fun acc element -> StringPQ.enqueue element acc)
    StringPQ.empty elements

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
  let q = enqueue_elements [ "test"; "work"; "satoru"; "reigen" ] in
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
  let q = enqueue_elements [ "test"; "works"; "work" ] in
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
  let q = enqueue_elements [ "test"; "test2" ] in
  let q = StringPQ.dequeue q in
  assert_equal [ "test2" ] (StringPQ.to_list q) ~printer:(String.concat "; ")

let test_string_pq =
  "Testing sample test case StringPQ" >:: fun _ ->
  let q = enqueue_elements [ "hello"; "world"; "this"; "is"; "a"; "test" ] in
  assert_equal
    [ "a"; "is"; "this"; "test"; "hello"; "world" ]
    (StringPQ.to_list q) ~printer:(String.concat "; ")

(** [string_of_patient_lst lst] is the string format of a list of patients. *)
let string_of_patient_lst lst =
  String.concat "\n"
    (List.map
       (fun patient ->
         "[" ^ A4.Patient.name patient ^ "; "
         ^ A4.Patient.diagnosis patient
         ^ "]")
       lst)

(* Test cases *)

(* Requirement 3.3 and 3.4 *)
let test_patient_list_pq file =
  "Testing PatientListPQ" >:: fun _ ->
  let patients = Csv.load file in
  (* Load patients into a Patient.t list *)
  let og_patients =
    List.map
      (fun patient ->
        match patient with
        | [ name; diagnosis ] -> A4.Patient.create name diagnosis
        | _ -> A4.Patient.empty)
      patients
  in
  (* Load patients into initially empty PatientListPQ *)
  let new_patients =
    List.fold_left
      (fun acc patient -> PatientListPQ.enqueue patient acc)
      PatientListPQ.empty og_patients
  in
  (* Convert new_patients to a list of lists of strings *)
  let new_patients = PatientListPQ.to_list new_patients in

  (* 'assert_equal og_patients new_patients ~printer:string_of_patient_lst' was
     the code for Requirement 3.3.

     The following is the new code for Requirement 3.4 to account for Triage. *)
  let results =
    [
      A4.Patient.create "Jennifer Lawrence" "Appendicitis";
      A4.Patient.create "Tom Holland" "Appendicitis";
      A4.Patient.create "Timothee Chalamet" "Sprain";
      A4.Patient.create "Margot Robbie" "Sprain";
      A4.Patient.create "Austin Butler" "Flu";
      A4.Patient.create "Zendaya" "Flu";
    ]
  in
  assert_equal results new_patients ~printer:string_of_patient_lst

(* Test suite *)
let tests =
  "test suite"
  >::: [
         (* Tests for MakeListPQ *)
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
         (* Tests for Patient *)
         test_patient_list_pq "../data/waiting_room.csv";
       ]

let _ = run_test_tt_main tests
