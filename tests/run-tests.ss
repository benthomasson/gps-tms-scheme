#!/usr/bin/env scheme-script
;; GPS test suite — classic PAIP problems adapted for TMS-backed GPS

(import (chezscheme) (gps engine) (ftl reasons))

;; ---- Minimal test framework ----

(define *tests-run* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)
(define *failures* '())

(define-syntax test
  (syntax-rules ()
    [(_ name body ...)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (guard (e [#t (set! *tests-failed* (+ *tests-failed* 1))
                     (set! *failures* (cons (cons name (condition-message e)) *failures*))
                     (display (string-append "  FAIL: " name "\n"))
                     (display (string-append "    " (condition-message e) "\n"))])
         body ...
         (set! *tests-passed* (+ *tests-passed* 1))
         (display (string-append "  PASS: " name "\n"))))]))

(define (assert-equal actual expected msg)
  (unless (equal? actual expected)
    (error 'assert-equal
      (format "~a: expected ~s, got ~s" msg expected actual))))

(define (assert-true val msg)
  (unless val
    (error 'assert-true (format "~a: expected true, got ~s" msg val))))

(define (assert-false val msg)
  (unless (not val)
    (error 'assert-false (format "~a: expected false, got ~s" msg val))))

(define (assert-member item lst msg)
  (unless (member item lst)
    (error 'assert-member
      (format "~a: ~s not found in ~s" msg item lst))))

(define (assert-set-equal actual expected msg)
  (let ([a (list-sort string<? actual)]
        [e (list-sort string<? expected)])
    (unless (equal? a e)
      (error 'assert-set-equal
        (format "~a: expected ~s, got ~s" msg e a)))))

(define (node-tv net id)
  (node-truth-value (hashtable-ref (network-nodes net) id #f)))

(define (list-index lst item)
  (let loop ([l lst] [i 0])
    (cond
      [(null? l) -1]
      [(equal? (car l) item) i]
      [else (loop (cdr l) (+ i 1))])))

;; ---- Problem Definitions ----

;; PAIP Chapter 4: Drive son to school
(define school-ops
  (list
    (make-op "drive-son-to-school"
      '("son-at-home" "car-works")
      '("son-at-school")
      '("son-at-home"))
    (make-op "shop-installs-battery"
      '("car-needs-battery" "shop-knows-problem" "shop-has-money")
      '("car-works")
      '())
    (make-op "tell-shop-problem"
      '("in-communication-with-shop")
      '("shop-knows-problem")
      '())
    (make-op "telephone-shop"
      '("know-phone-number")
      '("in-communication-with-shop")
      '())
    (make-op "look-up-number"
      '("have-phone-book")
      '("know-phone-number")
      '())
    (make-op "give-shop-money"
      '("have-money")
      '("shop-has-money")
      '("have-money"))))

;; Monkey and bananas
(define banana-ops
  (list
    (make-op "push-chair-to-middle"
      '("chair-at-door" "at-door")
      '("chair-at-middle-room" "at-middle-room")
      '("chair-at-door" "at-door"))
    (make-op "climb-on-chair"
      '("chair-at-middle-room" "at-middle-room" "on-floor")
      '("at-bananas" "on-chair")
      '("at-middle-room" "on-floor"))
    (make-op "grasp-bananas"
      '("at-bananas")
      '("has-bananas")
      '())
    (make-op "eat-bananas"
      '("has-bananas" "hungry")
      '("not-hungry")
      '("has-bananas" "hungry"))))

;; Simple logistics
(define logistics-ops
  (list
    (make-op "load-truck"
      '("package-at-A" "truck-at-A")
      '("package-in-truck")
      '("package-at-A"))
    (make-op "drive-A-to-B"
      '("truck-at-A")
      '("truck-at-B")
      '("truck-at-A"))
    (make-op "unload-truck"
      '("package-in-truck" "truck-at-B")
      '("package-at-B")
      '("package-in-truck"))))

;; ---- TestSchoolProblem ----

(display "\n=== TestSchoolProblem ===\n")

(test "school_finds_plan"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
      '("son-at-school")
      school-ops)])
    (assert-true (list? plan) "found a plan")
    (assert-member "son-at-school" final "goal in final state")
    (assert-member "drive-son-to-school" plan "drive in plan")))

(test "school_plan_order"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
      '("son-at-school")
      school-ops)])
    (assert-member "look-up-number" plan "look up number")
    (assert-member "telephone-shop" plan "telephone shop")
    (assert-member "give-shop-money" plan "give money")
    (assert-member "tell-shop-problem" plan "tell problem")
    (assert-member "shop-installs-battery" plan "install battery")
    ;; drive must come after install-battery
    (let ([drive-pos (list-index plan "drive-son-to-school")]
          [install-pos (list-index plan "shop-installs-battery")])
      (assert-true (> drive-pos install-pos) "drive after install"))))

(test "school_no_phone_book_fails"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money")
      '("son-at-school")
      school-ops)])
    (assert-false plan "no plan without phone book")))

(test "school_clobbered_sibling_goal"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
      '("have-money" "son-at-school")
      school-ops)])
    (assert-false plan "money clobbered by give-shop-money")))

;; ---- TestBananaProblem ----

(display "\n=== TestBananaProblem ===\n")

(test "banana_finds_plan"
  (let-values ([(plan final) (gps
      '("at-door" "on-floor" "has-ball" "hungry" "chair-at-door")
      '("not-hungry")
      banana-ops)])
    (assert-true (list? plan) "found a plan")
    (assert-member "not-hungry" final "goal achieved")))

(test "banana_plan_steps"
  (let-values ([(plan final) (gps
      '("at-door" "on-floor" "has-ball" "hungry" "chair-at-door")
      '("not-hungry")
      banana-ops)])
    (assert-member "push-chair-to-middle" plan "push chair")
    (assert-member "climb-on-chair" plan "climb")
    (assert-member "grasp-bananas" plan "grasp")
    (assert-member "eat-bananas" plan "eat")))

(test "banana_not_hungry_without_chair_fails"
  (let-values ([(plan final) (gps
      '("at-door" "on-floor" "hungry")
      '("not-hungry")
      banana-ops)])
    (assert-false plan "can't reach bananas without chair")))

;; ---- TestLogistics ----

(display "\n=== TestLogistics ===\n")

(test "logistics_deliver_package"
  (let-values ([(plan final) (gps
      '("package-at-A" "truck-at-A")
      '("package-at-B")
      logistics-ops)])
    (assert-true (list? plan) "found a plan")
    (assert-equal plan '("load-truck" "drive-A-to-B" "unload-truck") "correct order")))

(test "logistics_truck_wrong_location_fails"
  (let-values ([(plan final) (gps
      '("package-at-A" "truck-at-B")
      '("package-at-B")
      logistics-ops)])
    (assert-false plan "truck not at A")))

;; ---- TestEdgeCases ----

(display "\n=== TestEdgeCases ===\n")

(test "goal_already_achieved"
  (let-values ([(plan final) (gps '("at-home" "happy") '("happy") '())])
    (assert-true (list? plan) "plan exists")
    (assert-equal plan '() "empty plan")))

(test "multiple_goals_already_achieved"
  (let-values ([(plan final) (gps '("a" "b" "c") '("a" "c") '())])
    (assert-equal plan '() "empty plan")))

(test "impossible_goal"
  (let-values ([(plan final) (gps '("a") '("b") '())])
    (assert-false plan "no operators to achieve b")))

(test "empty_goals"
  (let-values ([(plan final) (gps '("a" "b") '() '())])
    (assert-equal plan '() "empty plan")))

(test "loop_detection"
  (let-values ([(plan final) (gps '("a")
      '("c")
      (list (make-op "op1" '("b") '("c") '())
            (make-op "op2" '("c") '("b") '())))])
    (assert-false plan "circular dependency detected")))

;; ---- TestTMSNetwork ----

(display "\n=== TestTMSNetwork ===\n")

(test "tms_school_goals_in"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
      '("son-at-school")
      school-ops)])
    (let ([net (gps->network
            '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
            plan school-ops)])
      (assert-equal (node-tv net "son-at-school") "IN" "goal is IN"))))

(test "tms_school_actions_in"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
      '("son-at-school")
      school-ops)])
    (let ([net (gps->network
            '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
            plan school-ops)])
      (assert-equal (node-tv net "do:drive-son-to-school") "IN" "drive action IN")
      (assert-equal (node-tv net "do:look-up-number") "IN" "look-up action IN"))))

(test "tms_school_explain_traces_to_premises"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
      '("son-at-school")
      school-ops)])
    (let* ([net (gps->network
             '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
             plan school-ops)]
           [assumptions (network-trace-assumptions net "son-at-school")])
      (assert-member "son-at-home" assumptions "traces to son-at-home")
      (assert-member "have-phone-book" assumptions "traces to have-phone-book"))))

(test "tms_retract_initial_cascades"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
      '("son-at-school")
      school-ops)])
    (let ([net (gps->network
            '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
            plan school-ops)])
      (network-retract! net "have-phone-book")
      (assert-equal (node-tv net "do:look-up-number") "OUT"
                    "look-up cascades out")
      (assert-equal (node-tv net "know-phone-number") "OUT"
                    "know-number cascades out")
      (assert-equal (node-tv net "do:telephone-shop") "OUT"
                    "telephone cascades out")
      (assert-equal (node-tv net "son-at-school") "OUT"
                    "goal cascades out"))))

(test "tms_retract_and_restore"
  (let-values ([(plan final) (gps
      '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
      '("son-at-school")
      school-ops)])
    (let ([net (gps->network
            '("son-at-home" "car-needs-battery" "have-money" "have-phone-book")
            plan school-ops)])
      (network-retract! net "have-money")
      (assert-equal (node-tv net "son-at-school") "OUT" "goal out")
      (network-assert-node! net "have-money")
      (assert-equal (node-tv net "son-at-school") "IN" "goal restored"))))

(test "tms_logistics_explain"
  (let-values ([(plan final) (gps
      '("package-at-A" "truck-at-A")
      '("package-at-B")
      logistics-ops)])
    (let* ([net (gps->network '("package-at-A" "truck-at-A") plan logistics-ops)]
           [steps (network-explain net "package-at-B")])
      (assert-true (> (length steps) 1) "multi-step explanation")
      (assert-equal (alist-ref "truth_value" (car steps) "") "IN" "goal is IN"))))

(test "tms_banana_what_if"
  (let-values ([(plan final) (gps
      '("at-door" "on-floor" "has-ball" "hungry" "chair-at-door")
      '("not-hungry")
      banana-ops)])
    (let ([net (gps->network
            '("at-door" "on-floor" "has-ball" "hungry" "chair-at-door")
            plan banana-ops)])
      (assert-equal (node-tv net "not-hungry") "IN" "goal in")
      ;; What if the chair wasn't at the door?
      (network-retract! net "chair-at-door")
      (assert-equal (node-tv net "not-hungry") "OUT" "no chair, no bananas"))))

;; ---- Summary ----

(newline)
(display "==================================\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(unless (null? *failures*)
  (display "\nFailures:\n")
  (for-each (lambda (f)
              (display (format "  ~a: ~a\n" (car f) (cdr f))))
            (reverse *failures*)))
(display "==================================\n")

(when (> *tests-failed* 0)
  (exit 1))
