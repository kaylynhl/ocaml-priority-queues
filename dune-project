(lang dune 3.16)

(name triage)

(generate_opam_files true)

(source (github kaylynhl/ocaml-priority-queues))

(authors "Kaylyn Lee")

(maintainers "Kaylyn Lee")

(license "MIT")

(package
 (name triage)
 (synopsis "Priority-queue driven hospital triage simulator in OCaml")
 (description
  "A small OCaml project that models a hospital waiting room using reusable \
   priority queue functors and an interactive CLI.")
 (depends
  ocaml
  dune
  csv
  (ounit2 :with-test))
 (tags (priority-queue triage ocaml functors cli)))