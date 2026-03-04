(** A priority queue stores elements with integer priorities. Lower numbers mean
    higher priority. *)
module type PriorityQueue = sig
  type elt
  (** Element type. *)

  type t
  (** Queue representation. *)

  exception Empty
  (** Raised when [front] or [dequeue] is called on an empty queue. *)

  val empty : t
  (** The empty queue. *)

  val is_empty : t -> bool
  (** [is_empty q] checks whether [q] has no elements. *)

  val enqueue : elt -> t -> t
  (** [enqueue x q] inserts [x] while preserving queue ordering. *)

  val front : t -> elt
  (** [front q] returns the highest-priority element of [q]. *)

  val dequeue : t -> t
  (** [dequeue q] removes the highest-priority element of [q]. *)

  val to_list : t -> elt list
  (** [to_list q] returns elements in the order they would be removed. *)
end

module type Prioritizable = sig
  type t

  val priority : t -> int
  (** [priority x] is a non-negative priority where smaller is higher. *)
end

(** List-backed queue. Complexity:
    - [enqueue]: O(n)
    - [front], [dequeue]: O(1)
    - [to_list]: O(1) *)
module MakeListPQ (T : Prioritizable) : PriorityQueue with type elt = T.t

(** Heap-like tree queue. Complexity:
    - [enqueue], [dequeue]: amortized O(log n)
    - [front]: O(1)
    - [to_list]: O(n log n) because it repeatedly merges during extraction. *)
module MakeTreePQ (T : Prioritizable) : PriorityQueue with type elt = T.t
