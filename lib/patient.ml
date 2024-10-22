type t = string list

exception Error

let empty = []

let create name diagnosis =
  match name with
  | "" -> raise Error
  | _ -> (
      match diagnosis with
      | "Appendicitis" | "Sprain" | "Flu" -> [ name; diagnosis ]
      | _ -> raise Error)

let name patient =
  match patient with
  | n :: _ -> n
  | [] -> raise Error

let diagnosis patient =
  match patient with
  | _ :: d :: _ -> d
  | _ -> raise Error

let priority patient =
  let diagnosis = diagnosis patient in
  match diagnosis with
  | "Appendicitis" -> 0
  | "Sprain" -> 1
  | "Flu" -> 2
  | _ -> raise Error
