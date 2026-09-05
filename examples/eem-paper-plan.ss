#!/usr/bin/env scheme-script
;; GPS planning: get EEM paper accepted at a conference
;; First real use of GPS for planning actual work

(import (chezscheme) (gps engine) (ftl reasons))

;; ---- Initial state: what we have right now ----
(define initial-state
  '("experimental-results-exist"
    "literature-survey-started"
    "novelty-claims-verified"
    "neusymms-overlap-resolved"
    "eem-framing-established"
    "ftl-reasons-implemented"
    "expert-service-implemented"
    "expert-knowledge-bases-exist"
    "kai-xu-feedback-received"
    "beliefs-pi-entries-written"))

;; ---- Goal ----
(define goal '("paper-accepted-at-conference"))

;; ---- Operators ----
(define paper-ops
  (list
    (make-op "identify-target-conference"
      '("kai-xu-feedback-received")
      '("target-conference-identified")
      '())

    (make-op "complete-literature-survey"
      '("literature-survey-started" "neusymms-overlap-resolved")
      '("literature-survey-complete")
      '("literature-survey-started"))

    (make-op "identify-experimental-gaps"
      '("experimental-results-exist" "literature-survey-complete")
      '("experimental-gaps-identified")
      '())

    (make-op "run-additional-experiments"
      '("experimental-gaps-identified" "ftl-reasons-implemented")
      '("experimental-gaps-filled")
      '("experimental-gaps-identified"))

    (make-op "write-related-work"
      '("literature-survey-complete")
      '("related-work-written")
      '())

    (make-op "write-experiment-section"
      '("experimental-results-exist" "experimental-gaps-filled")
      '("experiment-section-written")
      '())

    (make-op "write-system-description"
      '("ftl-reasons-implemented" "expert-service-implemented" "eem-framing-established")
      '("system-description-written")
      '())

    (make-op "write-introduction"
      '("novelty-claims-verified" "eem-framing-established")
      '("introduction-written")
      '())

    (make-op "assemble-paper-draft"
      '("introduction-written" "related-work-written"
        "system-description-written" "experiment-section-written")
      '("paper-draft-complete")
      '())

    (make-op "internal-review"
      '("paper-draft-complete")
      '("paper-reviewed")
      '())

    (make-op "revise-from-internal-review"
      '("paper-reviewed")
      '("paper-revised")
      '("paper-reviewed"))

    (make-op "check-formatting-requirements"
      '("target-conference-identified")
      '("formatting-requirements-known")
      '())

    (make-op "format-for-submission"
      '("paper-revised" "formatting-requirements-known")
      '("paper-formatted")
      '())

    (make-op "submit-paper"
      '("paper-formatted" "target-conference-identified")
      '("paper-submitted")
      '("paper-formatted"))

    (make-op "receive-reviews"
      '("paper-submitted")
      '("reviews-received")
      '())

    (make-op "revise-from-reviews"
      '("reviews-received" "paper-draft-complete")
      '("revision-submitted")
      '("reviews-received"))

    (make-op "paper-accepted"
      '("revision-submitted")
      '("paper-accepted-at-conference")
      '())))

;; ---- Run GPS ----
(display "=== GPS: Plan to get EEM paper accepted ===\n\n")

(let-values ([(plan final) (gps initial-state goal paper-ops)])
  (if plan
      (begin
        (display "Plan found!\n\n")
        (display "Steps:\n")
        (let loop ([steps plan] [i 1])
          (unless (null? steps)
            (display (format "  ~a. ~a\n" i (car steps)))
            (loop (cdr steps) (+ i 1))))

        (display (format "\nTotal steps: ~a\n\n" (length plan)))

        ;; Build TMS network
        (display "=== TMS Analysis ===\n\n")
        (let ([net (gps->network initial-state plan paper-ops)])

          ;; What are the critical assumptions?
          (let ([assumptions (network-trace-assumptions net "paper-accepted-at-conference")])
            (display "Critical assumptions (initial conditions the goal depends on):\n")
            (for-each (lambda (a)
                        (display (format "  - ~a\n" a)))
                      (list-sort string<? assumptions)))

          (newline)

          ;; Sensitivity analysis: retract each initial condition in turn
          (display "Sensitivity analysis:\n")
          (for-each
            (lambda (cond)
              (let* ([snap (string-append "pre:" cond)]
                     [node (or (hashtable-ref (network-nodes net) snap #f)
                               (hashtable-ref (network-nodes net) cond #f))])
                (when node
                  (let ([id (if (hashtable-ref (network-nodes net) snap #f) snap cond)])
                    (network-retract! net id)
                    (let ([goal-node (hashtable-ref (network-nodes net)
                                       "paper-accepted-at-conference" #f)])
                      (display (format "  ~a removed → goal ~a\n"
                                cond
                                (if goal-node
                                    (node-truth-value goal-node)
                                    "missing"))))
                    (network-assert-node! net id)))))
            initial-state)))

      (display "No plan found!\n")))
