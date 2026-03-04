# Hospital Triage Simulator (OCaml)

A portfolio-ready OCaml project that models a hospital waiting room with a priority queue.
Patients are treated by diagnosis urgency:

> Note: This project originated as a coursework codebase and has since been refactored and documented for clarity, testing, and maintainability.

1. `Appendicitis`
2. `Sprain`
3. `Flu`

Smaller numeric priorities are treated first, and ties preserve admission order.

## Highlights

- Reusable priority-queue abstraction via functors
- Two queue implementations (list-backed and tree-backed)
- Typed patient domain model with input validation
- Interactive CLI for admitting, previewing, and treating patients
- OUnit test suite covering queue behavior and patient invariants

## Project Structure

- `lib/patient.ml`: Patient domain model and triage priority logic
- `lib/priorityQueues.ml`: Priority queue signature + functor implementations
- `bin/main.ml`: Interactive waiting-room simulator
- `test/test_triage.ml`: Unit tests and behavioral checks
- `data/waiting_room.csv`: Sample initial waiting-room data

## Build and Run

```bash
dune build
```

Or, using helper targets:

```bash
make build
```

Run interactively:

```bash
dune exec triage
```

Run with initial patient data from CSV:

```bash
dune exec triage -- data/waiting_room.csv
```

## Test

```bash
dune runtest
```

Or:

```bash
make test
```

## Format

This repo uses `.ocamlformat` for style consistency.

```bash
make fmt
```

## Example Session

```text
What would you like to do next? Type one of: preview, admit, treat, quit:
admit
Input diagnosis + name separated by spaces, e.g. 'Sprain Reigen Arataka':
Appendicitis Jane Doe
Patient Jane Doe with Appendicitis diagnosis successfully admitted to the waiting room.
```

## CSV Format

Expected format is one patient per row:

```text
<name>,<diagnosis>
```

Example:

```text
Jennifer Lawrence,Appendicitis
Tom Holland,Appendicitis
Timothee Chalamet,Sprain
```

Rows with invalid data are skipped when loading.
