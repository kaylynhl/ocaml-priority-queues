type diagnosis =
  | Appendicitis
  | Sprain
  | Flu

type t = {
  name : string option;
  diagnosis : diagnosis option;
}

exception Error

let empty = { name = None; diagnosis = None }
let normalize_name name = String.trim name

let diagnosis_of_string = function
  | "Appendicitis" -> Appendicitis
  | "Sprain" -> Sprain
  | "Flu" -> Flu
  | _ -> raise Error

let string_of_diagnosis = function
  | Appendicitis -> "Appendicitis"
  | Sprain -> "Sprain"
  | Flu -> "Flu"

let create name diagnosis =
  let normalized_name = normalize_name name in
  if normalized_name = "" then raise Error
  else
    let parsed_diagnosis = diagnosis_of_string diagnosis in
    { name = Some normalized_name; diagnosis = Some parsed_diagnosis }

let get_option_field = function
  | Some value -> value
  | None -> raise Error

let name patient = get_option_field patient.name

let diagnosis patient =
  let diagnosis = get_option_field patient.diagnosis in
  string_of_diagnosis diagnosis

let priority patient =
  match get_option_field patient.diagnosis with
  | Appendicitis -> 0
  | Sprain -> 1
  | Flu -> 2
(* NOTE: We intentionally keep [empty] + field accessors that can raise [Error]
   because tests and CLI behavior depend on this legacy API contract. *)
