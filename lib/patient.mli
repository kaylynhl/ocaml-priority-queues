type t
(** The type of a patient *)

exception Error
(** Raised if there is an error with patient information. *)

val empty : t
(** No patient information *)

val create : string -> string -> t
(** [create name diagnosis] is the patient with the name [name] and malady
    [diagnosis]. Requires: [name] is a non-empty string and [diagnosis] is one
    of the following strings: "Appendicitis", "Sprain", or "Flu".
    Case-sensitive. Raises [Error] exception if not. *)

val name : t -> string
(** [name patient] is the name of the patient. Raises [Error] exception if
    patient has no name. *)

val diagnosis : t -> string
(** [diagnosis patient] is the malady that the patient has been diagnosed with.
    Raises [Error] exception if patient has no diagnosis. *)

val priority : t -> int
(** [priority patient] is the priority level of the patient based on their
    malady. A diagnosis of "Appendicitis" returns 0, "Sprain" returns 1, and
    "Flu" returns 2, where the smaller the int the higher the priority. Raises
    [Error] exception if diagnosis of [patient] is invalid. *)
