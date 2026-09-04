# CLAUDE.md

## What This Is

TMS-backed General Problem Solver (GPS) in Chez Scheme. Implements Newell & Simon's means-ends analysis (as presented in Norvig's PAIP Chapter 4) with a Truth Maintenance System providing plan justification, what-if analysis, and retraction cascading.

Depends on [reasons-scheme](https://github.com/benthomasson/reasons-scheme) for the `(ftl reasons)` TMS library.

## Project Structure

```
lib/gps/
  engine.sls             # (gps engine) — GPS solver + TMS plan execution
tests/
  run-tests.ss           # 21 tests: school, banana, logistics, edge cases, TMS
```

## Running Tests

```bash
scheme --libdirs "lib:../reasons-scheme/lib" --script tests/run-tests.ss
```

## Using the Library

```bash
scheme --libdirs "lib:../reasons-scheme/lib"
```

```scheme
(import (gps engine) (ftl reasons))

;; Define operators
(define ops
  (list
    (make-op "drive" '("has-car" "has-fuel") '("at-destination") '("at-home"))
    (make-op "refuel" '("has-money") '("has-fuel") '("has-money"))))

;; Solve
(let-values ([(plan final) (gps '("at-home" "has-car" "has-money") '("at-destination") ops)])
  (display plan))
;; => ("refuel" "drive")

;; Build TMS explanation network
(let-values ([(plan final) (gps '("at-home" "has-car" "has-money") '("at-destination") ops)])
  (let ([net (gps->network '("at-home" "has-car" "has-money") plan ops)])
    ;; What initial conditions does the goal depend on?
    (network-trace-assumptions net "at-destination")
    ;; => ("has-car" "has-money")
    ;; What breaks if we remove has-money?
    (network-retract! net "has-money")
    (node-truth-value (hashtable-ref (network-nodes net) "at-destination" #f))
    ;; => "OUT"
    ))
```

## Requirements

- Chez Scheme 10.x
- reasons-scheme (sibling directory `../reasons-scheme`)

## Design

Two-phase architecture:
1. **Search** — Pure means-ends analysis finds a plan (sequence of operators). Handles loop detection and clobbered-sibling-goal detection.
2. **TMS execution** — Builds a belief network explaining the plan. Initial conditions are premises; operators are justified by preconditions; effects are justified by operators. The TMS enables explain traces, assumption tracking, and retraction what-if analysis.
