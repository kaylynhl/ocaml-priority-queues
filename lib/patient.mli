type t
(** The type of a patient *)

exception Empty of string
(** Raised if there is no patient information. *)

val empty : t
(** No patient information *)

val create : string -> string -> t
(** [create name diagnosis] is the patient with the name [name] and malady
    [diagnosis] *)

val priority : t -> int
(** [priority patient] is the priority level of the patient based on their
    malady. *)

val name : t -> string
(** [name patient] is the name of the patient. *)

val diagnosis : t -> string
(** [diagnosis patient] is the malady that the patient has been diagnosed with. *)
