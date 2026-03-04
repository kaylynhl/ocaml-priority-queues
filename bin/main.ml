open Triage
module PatientPQ = PriorityQueues.MakeListPQ (Patient)

let prompt_separator () = print_endline "---------"
let waiting_room_empty_message () = print_endline "The waiting room is empty!"

let preview_queue q =
  if PatientPQ.is_empty q then waiting_room_empty_message ()
  else
    let names =
      PatientPQ.to_list q |> List.map Patient.name |> String.concat ", "
    in
    print_endline
      "Names of patients in the waiting room in the order they will be seen:";
    print_endline names

let admit diagnosis patient_name q =
  try
    let patient = Patient.create patient_name diagnosis in
    print_endline
      ("Patient " ^ Patient.name patient ^ " with " ^ Patient.diagnosis patient
     ^ " diagnosis successfully admitted to the waiting room.");
    PatientPQ.enqueue patient q
  with Patient.Error ->
    print_endline
      "Invalid input. Diagnosis must be one of Flu/Appendicitis/Sprain and \
       name must be non-empty.";
    q

let parse_admit_input input =
  match Str.split (Str.regexp "[ \t]+") (String.trim input) with
  | [] -> None
  | diagnosis :: name_tokens -> Some (diagnosis, String.concat " " name_tokens)

let rec prompt_user q =
  prompt_separator ();
  print_endline
    "What would you like to do next? Type one of: preview, admit, treat, quit:";
  print_endline "'preview' to view waiting-room order.";
  print_endline "'admit' to add a patient to the waiting room.";
  print_endline "'treat' to simulate the doctor seeing the next patient.";
  print_endline "'quit' to exit the program.";
  prompt_separator ();
  match read_line () with
  | "preview" ->
      preview_queue q;
      prompt_user q
  | "admit" -> (
      print_endline
        "Input diagnosis + name separated by spaces, e.g. 'Sprain Reigen \
         Arataka':";
      match parse_admit_input (read_line ()) with
      | Some (diagnosis, patient_name) ->
          let updated_q = admit diagnosis patient_name q in
          prompt_user updated_q
      | _ ->
          print_endline "Invalid input.";
          prompt_user q)
  | "treat" ->
      if PatientPQ.is_empty q then (
        waiting_room_empty_message ();
        prompt_user q)
      else
        let next_patient = PatientPQ.front q in
        let updated_q = q |> PatientPQ.dequeue in
        print_endline
          ("The next patient is " ^ Patient.name next_patient
         ^ " with diagnosis "
          ^ Patient.diagnosis next_patient
          ^ ". They were removed from the waiting room.");
        prompt_user updated_q
  | "quit" ->
      print_endline "Exiting program. Bye!";
      exit 0
  | _ ->
      print_endline "Invalid choice. Please try again.";
      prompt_user q

let load_patients_from_csv filename =
  let rows = Csv.load filename in
  List.fold_left
    (fun q row ->
      match row with
      | [ name; diagnosis ] -> (
          try PatientPQ.enqueue (Patient.create name diagnosis) q
          with Patient.Error -> q)
      | _ -> q)
    PatientPQ.empty rows

let () =
  let patient_pq = PatientPQ.empty in
  if Array.length Sys.argv = 1 then prompt_user patient_pq
  else if Array.length Sys.argv = 2 then
    try
      let patient_pq = load_patients_from_csv Sys.argv.(1) in
      prompt_user patient_pq
    with Sys_error _ | Csv.Failure _ ->
      print_endline "Error: could not read provided CSV file."
  else print_endline "Error: Must provide either 0 or 1 argument."
