# gps-tms-scheme

General Problem Solver backed by a Truth Maintenance System, implemented in Chez Scheme.

GPS (Newell & Simon 1963) uses means-ends analysis to find plans: given a current state, a goal state, and a set of operators with preconditions and effects, find a sequence of operators that achieves all goals. This implementation follows Norvig's PAIP Chapter 4 formulation and adds a TMS layer from [reasons-scheme](https://github.com/benthomasson/reasons-scheme) for plan justification and what-if analysis.

## What the TMS adds

Classic GPS finds a plan. TMS-backed GPS also explains it:

```scheme
(import (gps engine) (ftl reasons))

;; Find a plan to get son to school (PAIP Chapter 4)
(let-values ([(plan final)
  (gps '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
       '("son-at-school")
       school-ops)])

  ;; plan => ("look-up-number" "telephone-shop" "give-shop-money"
  ;;          "tell-shop-problem" "shop-installs-battery" "drive-son-to-school")

  ;; Build the justification network
  (let ([net (gps->network
          '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
          plan school-ops)])

    ;; Trace goal back to initial conditions
    (network-trace-assumptions net "son-at-school")
    ;; => ("son-at-home" "car-needs-battery" "have-money" "have-phone-book")

    ;; What-if: what breaks without a phone book?
    (network-retract! net "have-phone-book")
    (node-truth-value (hashtable-ref (network-nodes net) "son-at-school" #f))
    ;; => "OUT" — the entire plan collapses

    ;; Restore it — plan recovers automatically
    (network-assert-node! net "have-phone-book")
    (node-truth-value (hashtable-ref (network-nodes net) "son-at-school" #f))
    ;; => "IN"
    ))
```

The TMS provides:
- **Explain traces** -- `network-explain` shows the full reasoning chain from any goal back to initial conditions through the operators
- **Assumption tracking** -- `network-trace-assumptions` identifies which initial conditions a goal depends on
- **What-if analysis** -- retract an initial condition and the TMS automatically cascades, showing exactly which parts of the plan break
- **Automatic restoration** -- re-assert the condition and everything recovers

## Requirements

- [Chez Scheme](https://cisco.github.io/ChezScheme/) 10.x
- [reasons-scheme](https://github.com/benthomasson/reasons-scheme) as a sibling directory

## Quick start

```bash
git clone https://github.com/benthomasson/reasons-scheme.git
git clone https://github.com/benthomasson/gps-tms-scheme.git
cd gps-tms-scheme
scheme --libdirs "lib:../reasons-scheme/lib" --script tests/run-tests.ss
```

## Defining operators

An operator has a name, preconditions, add-list (effects), and delete-list:

```scheme
(make-op "drive-son-to-school"
  '("son-at-home" "car-works")     ; preconditions
  '("son-at-school")               ; add-list (becomes true)
  '("son-at-home"))                ; del-list (becomes false)
```

The GPS solver tries operators whose add-list contains an unsatisfied goal, recursively achieves their preconditions, and applies state changes.

## Features

- **Means-ends analysis** -- backward chaining from goals to find applicable operators
- **Loop detection** -- prevents infinite recursion when operator preconditions are circular
- **Clobbered sibling goal detection** -- detects when achieving one goal destroys another
- **TMS plan justification** -- `gps->network` builds a belief network explaining the plan
- **Classic PAIP problems** -- school, monkey-and-bananas, logistics included as tests

## Running tests

```bash
scheme --libdirs "lib:../reasons-scheme/lib" --script tests/run-tests.ss
```

21 tests covering the GPS solver (school problem, banana problem, logistics, edge cases) and TMS integration (explain traces, retraction cascading, what-if analysis).

## API

| Function | Description |
|---|---|
| `(make-op action preconds add-list del-list)` | Create an operator |
| `(gps initial-state goals ops)` | Find a plan; returns `(values plan final-state)` or `(values #f #f)` |
| `(gps->network initial-state plan ops)` | Build a TMS network explaining the plan |

## Architecture

```
                    GPS Search (Phase 1)
                    --------------------
Initial state  -->  means-ends analysis  -->  plan
+ goals             with loop detection       (list of actions)
+ operators         and clobber detection

                    TMS Execution (Phase 2)
                    -----------------------
Initial state  -->  build justification  -->  belief network
+ plan               chain on TMS             (explain, retract,
+ operators                                    what-if analysis)
```

Phase 1 is pure search -- no TMS needed. Phase 2 records the plan's reasoning structure in the TMS, enabling analysis that classic GPS can't do.

## Connection to Doyle 1979

This is Doyle's architecture made literal: the TMS holds the state, the problem solver (GPS) drives the search. Justification chains encode operator-precondition relationships. The TMS propagation engine handles state consistency. Retraction cascades show plan fragility. This is exactly what a Truth Maintenance System was designed for -- maintaining justified beliefs under change.

## License

MIT
