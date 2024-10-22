open A4

(* Make PatientPQ *)
module PatientPQ = PriorityQueues.MakeListPQ (Patient)

let admit diagnosis patient_name q =
  match diagnosis with
  | "Appendicitis" | "Sprain" | "Flu" -> (
      match patient_name with
      | "" ->
          print_endline
            "Invalid name! Names must be non-empty. Returning to the beginning.";
          q
      | name ->
          print_endline
            ("Patient " ^ patient_name ^ " with " ^ diagnosis
           ^ " diagnosis successfully admitted to the waiting room!");
          PatientPQ.enqueue (Patient.create patient_name diagnosis) q)
  | _ ->
      print_endline "Invalid diagnosis! Returning to the beginning.";
      q

let rec prompt_user q =
  print_endline "---------";
  print_endline
    "What would you like to do next? Please type either 'preview', 'admit', \
     'treat', or 'quit': ";
  print_endline
    "'preview' to view the names of patients in the waiting room in the order \
     they will be seen.";
  print_endline
    "'admit' to simulate the entry of a new patient into the waiting room.";
  print_endline "'treat' to simulate the doctor seeing the next patient.";
  print_endline "'quit' to exit the program.";
  print_endline "---------";
  let user_input = read_line () in
  match user_input with
  | "preview" ->
      if PatientPQ.is_empty q then (
        print_endline "The waiting room is empty!";
        prompt_user q)
      else (
        print_endline
          "Names of patients in the waiting room in the order they will be \
           seen (e.g. The left-most name in the list will be seen next): ";
        let q_list = PatientPQ.to_list q in
        let patient_names =
          List.fold_right
            (fun patient acc -> Patient.name patient :: acc)
            q_list []
        in
        print_endline (String.concat ", " patient_names);
        prompt_user q)
  | "admit" -> (
      print_endline
        "Please input a valid diagnosis and non-empty name for the patient \
         separated with a space. Valid diagnoses are 'Flu', 'Appendicitis', \
         and 'Sprain'. Case-sensitive. Example: 'Sprain Reigen Arataka': ";
      let admit_input = read_line () in
      let substrings = String.split_on_char ' ' admit_input in
      match substrings with
      | diagnosis :: name ->
          let patient_name = String.concat " " name in
          let new_q = admit diagnosis patient_name q in
          prompt_user new_q
      | _ ->
          print_endline "Invalid input! Returning to the beginning.";
          prompt_user q)
  | "treat" ->
      if PatientPQ.is_empty q then (
        print_endline "The waiting room is empty!";
        prompt_user q)
      else
        let next_patient = PatientPQ.front q in
        let name = Patient.name next_patient in
        let diagnosis = Patient.diagnosis next_patient in
        let updated_q = PatientPQ.dequeue q in
        print_endline
          ("The name of the next patient is " ^ name
         ^ " and their diagnosis is " ^ diagnosis
         ^ "! They will be treated and be removed from the waiting room.");
        prompt_user updated_q
  | "quit" ->
      if PatientPQ.is_empty q = false then (
        print_endline "Waiting room is not empty. Exiting program. Bye!";
        exit 0)
      else (
        print_endline "Exiting program. Bye!";
        exit 0)
  | _ ->
      print_endline "Invalid choice :( Please try again.";
      prompt_user q

let () =
  let patient_pq = PatientPQ.empty in

  if Array.length Sys.argv = 1 then prompt_user patient_pq
  else if Array.length Sys.argv = 2 then
    try
      let patients = Csv.load Sys.argv.(1) in
      let patient_pq =
        List.fold_left
          (fun acc patient ->
            match patient with
            | [ name; diagnosis ] ->
                let new_patient = Patient.create name diagnosis in
                PatientPQ.enqueue new_patient acc
            | _ -> acc)
          patient_pq patients
      in
      prompt_user patient_pq
    with _ -> print_endline "Error: Can't access provided file."
  else print_endline "Error: Must provide either 0 or 1 argument."
