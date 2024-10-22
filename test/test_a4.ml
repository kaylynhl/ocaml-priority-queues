open OUnit2
open A4.PriorityQueues
open A4

(* Given module *)
module LengthPrioritizedString = struct
  include String

  (** The priority of a string is its length. Example: [priority "hello" = 5] *)
  let priority s = length s
end

(* Testing Functors *)
module StringPQTester
    (Q : PriorityQueue with type elt = LengthPrioritizedString.t) =
struct
  (** [enqueue_elements elements] enqueues each element of the list of elements
      [elements] to an empty StringPQ. *)
  let enqueue_elements elements =
    List.fold_left (fun acc element -> Q.enqueue element acc) Q.empty elements

  let test_is_empty_true =
    "Testing is_empty" >:: fun _ ->
    let q = Q.empty in
    assert_equal true (Q.is_empty q) ~printer:string_of_bool

  let test_enqueue_blackbox =
    "Blackbox testing enqueue" >:: fun _ ->
    let q = Q.enqueue "test" Q.empty in
    assert_equal false (Q.is_empty q) ~printer:string_of_bool

  let test_enqueue_same_priorities =
    "Testing enqueue" >:: fun _ ->
    let q = enqueue_elements [ "test"; "work"; "satoru"; "reigen" ] in
    assert_equal
      [ "test"; "work"; "satoru"; "reigen" ]
      (Q.to_list q) ~printer:(String.concat "; ")

  let test_enqueue =
    "Testing enqueue" >:: fun _ ->
    let q = Q.enqueue "test" Q.empty in
    assert_equal [ "test" ] (Q.to_list q) ~printer:(String.concat "; ")

  let test_front_empty =
    "Testing front with empty PQ raises error" >:: fun _ ->
    let q = Q.empty in
    assert_raises Q.Empty (fun _ -> Q.front q)

  let test_front =
    "Testing front" >:: fun _ ->
    let q = enqueue_elements [ "test"; "works"; "work" ] in
    assert_equal "test" (Q.front q) ~printer:Fun.id

  let test_dequeue_blackbox =
    "Blackbox testing dequeue" >:: fun _ ->
    let q = Q.enqueue "test" Q.empty in
    let q = Q.dequeue q in
    assert_equal true (Q.is_empty q) ~printer:string_of_bool

  let test_dequeue_empty =
    "Testing dequeue with empty PQ raises error" >:: fun _ ->
    let q = Q.empty in
    assert_raises Q.Empty (fun _ -> Q.dequeue q)

  let test_dequeue =
    "Testing dequeue" >:: fun _ ->
    let q = enqueue_elements [ "test"; "test2" ] in
    let q = Q.dequeue q in
    assert_equal [ "test2" ] (Q.to_list q) ~printer:(String.concat "; ")

  let test_string_pq =
    "Testing sample test case StringPQ" >:: fun _ ->
    let q = enqueue_elements [ "hello"; "world"; "this"; "is"; "a"; "test" ] in
    assert_equal
      [ "a"; "is"; "this"; "test"; "hello"; "world" ]
      (Q.to_list q) ~printer:(String.concat "; ")

  (* Test Suite *)
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
end

module PatientPQTester (Q : PriorityQueue with type elt = A4.Patient.t) = struct
  (** [string_of_patient_lst lst] is the string format of a list of patients. *)
  let string_of_patient_lst lst =
    String.concat "\n"
      (List.map
         (fun patient ->
           "[" ^ Patient.name patient ^ "; " ^ Patient.diagnosis patient ^ "]")
         lst)

  (* Requirement 3.3 and 3.4 *)
  let test_patient_list_pq file =
    "Testing PatientListPQ" >:: fun _ ->
    let patients = Csv.load file in
    (* Load patients into a Patient.t list *)
    let og_patients =
      List.map
        (fun patient ->
          match patient with
          | [ name; diagnosis ] -> Patient.create name diagnosis
          | _ -> Patient.empty)
        patients
    in
    (* Load patients into initially empty PatientListPQ *)
    let new_patients =
      List.fold_left
        (fun acc patient -> Q.enqueue patient acc)
        Q.empty og_patients
    in
    (* Convert new_patients to a list of lists of strings *)
    let new_patients = Q.to_list new_patients in

    (* 'assert_equal og_patients new_patients ~printer:string_of_patient_lst'
       was the code for Requirement 3.3.

       The following is the new code for Requirement 3.4 to account for
       Triage. *)
    let results =
      [
        Patient.create "Jennifer Lawrence" "Appendicitis";
        Patient.create "Tom Holland" "Appendicitis";
        Patient.create "Timothee Chalamet" "Sprain";
        Patient.create "Margot Robbie" "Sprain";
        Patient.create "Austin Butler" "Flu";
        Patient.create "Zendaya" "Flu";
      ]
    in
    assert_equal results new_patients ~printer:string_of_patient_lst

  let test_patient_errors case =
    "Testing patient error cases" >:: fun _ -> assert_raises Patient.Error case

  (* Test suite *)
  let tests =
    "test suite"
    >::: [
           test_patient_list_pq "../data/waiting_room.csv";
           test_patient_errors (fun () -> Patient.create "Gojo Satoru" "dead");
           test_patient_errors (fun () -> Patient.create "Reigen Arataka" "");
           test_patient_errors (fun () -> Patient.create "" "Appendicitis");
           test_patient_errors (fun () -> Patient.create "" "oogly boogly");
           test_patient_errors (fun () -> Patient.create "" "");
           test_patient_errors (fun () -> Patient.name Patient.empty);
           test_patient_errors (fun () -> Patient.diagnosis Patient.empty);
           test_patient_errors (fun () -> Patient.priority Patient.empty);
           test_patient_errors (fun () ->
               Patient.priority (Patient.create "Gojo Satoru" "oogly boogly"));
         ]
end

module StringListPQTest = StringPQTester (MakeListPQ (LengthPrioritizedString))
module PatientListPQTest = PatientPQTester (MakeListPQ (Patient))
module StringTreeTest = StringPQTester (MakeTreePQ (LengthPrioritizedString))
module PatientTreeTest = PatientPQTester (MakeTreePQ (Patient))

let tests =
  "test suite"
  >::: [
         StringListPQTest.tests;
         PatientListPQTest.tests;
         StringTreeTest.tests;
         PatientTreeTest.tests;
       ]

let _ = run_test_tt_main tests
