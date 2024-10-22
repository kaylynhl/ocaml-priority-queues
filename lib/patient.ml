type t = string list

exception Empty of string

let empty = []
let create name diagnosis = [ name; diagnosis ]

let name patient =
  match patient with
  | [] -> raise (Empty "Patient has empty name.")
  | n :: _ -> n

let diagnosis patient =
  match patient with
  | [] -> raise (Empty "Patient has empty diagnosis.")
  | _ :: d :: _ -> d
  | _ -> raise (Empty "Invalid patient information.")

let priority patient =
  let diagnosis = diagnosis patient in
  match diagnosis with
  | "Appendicitis" -> 0
  | "Sprain" -> 1
  | "Flu" -> 2
  | _ -> raise (Empty "Invalid patient diagnosis!")
