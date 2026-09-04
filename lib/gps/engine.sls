(library (gps engine)
  (export
    make-op op? op-action op-preconds op-add-list op-del-list
    gps gps->network)
  (import (chezscheme) (ftl reasons))

  (define-record-type op
    (fields action preconds add-list del-list))

  ;;; GPS solver — means-ends analysis with loop detection
  ;;;
  ;;; Phase 1: Search. Find a sequence of operators that transforms
  ;;; initial-state into a state containing all goals.
  ;;;
  ;;; Returns (values plan final-state) on success, (values #f #f) on failure.
  ;;; plan is a list of action name strings in execution order.

  (define (gps initial-state goals ops)
    (let ([result (achieve-all initial-state goals ops '())])
      (if (and result (subset? goals (car result)))
          (values (cdr result) (car result))
          (values #f #f))))

  (define (achieve-all state goals ops goal-stack)
    (let loop ([remaining goals] [st state] [plan '()])
      (if (null? remaining)
          (cons st plan)
          (let ([r (achieve st (car remaining) ops goal-stack)])
            (and r (loop (cdr remaining) (car r) (append plan (cdr r))))))))

  (define (achieve state goal ops goal-stack)
    (cond
      [(member goal state) (cons state '())]
      [(member goal goal-stack) #f]
      [else
       (try-ops (keep (lambda (o) (member goal (op-add-list o))) ops)
                state goal ops goal-stack)]))

  (define (try-ops candidates state goal ops goal-stack)
    (and (pair? candidates)
         (or (apply-op state goal (car candidates) ops goal-stack)
             (try-ops (cdr candidates) state goal ops goal-stack))))

  (define (apply-op state goal op ops goal-stack)
    (let ([r (achieve-all state (op-preconds op) ops (cons goal goal-stack))])
      (and r
           (let ([new-state (union (op-add-list op)
                                   (difference (car r) (op-del-list op)))])
             (cons new-state (append (cdr r) (list (op-action op))))))))

  ;;; Phase 2: TMS execution. Build a belief network that models
  ;;; the full plan including state transitions.
  ;;;
  ;;; Delete-lists use the TMS outlist mechanism: when an action goes IN,
  ;;; conditions it deletes go OUT. For circular cases (action depends on
  ;;; what it deletes), snapshot premises break the cycle.
  ;;;
  ;;; After execution, the network reflects the final state: deleted
  ;;; conditions are OUT, added conditions are IN. Retract a snapshot
  ;;; premise to see what breaks; the deleted condition comes back IN
  ;;; (the deletion is undone because the action is undone).

  (define (gps->network initial-state plan ops)
    (let ([net (make-network)]
          [idx (make-hashtable string-hash string=?)]
          [deleted-by (make-hashtable string-hash string=?)]
          [snap-map (make-hashtable string-hash string=?)]
          [created-by (make-hashtable string-hash string=?)])
      (for-each (lambda (o) (hashtable-set! idx (op-action o) o)) ops)

      ;; Track which action creates each condition (initial = #f)
      (for-each (lambda (c) (hashtable-set! created-by c #f)) initial-state)
      (for-each
        (lambda (action)
          (let ([op (hashtable-ref idx action #f)])
            (when op
              (for-each
                (lambda (c)
                  (unless (hashtable-contains? created-by c)
                    (hashtable-set! created-by c
                      (string-append "do:" action))))
                (op-add-list op)))))
        plan)

      ;; deleted-by: condition -> list of "do:action" that delete it
      (for-each
        (lambda (action)
          (let ([op (hashtable-ref idx action #f)])
            (when op
              (let ([aid (string-append "do:" action)])
                (for-each
                  (lambda (c)
                    (hashtable-update! deleted-by c
                      (lambda (xs) (cons aid xs)) '()))
                  (op-del-list op))))))
        plan)

      ;; snap-map: action -> ((cond . snap-id) ...) for volatile preconds.
      ;; Any precondition deleted by ANY action in the plan gets a snapshot
      ;; so the action's justification survives the deletion.
      (for-each
        (lambda (action)
          (let ([op (hashtable-ref idx action #f)])
            (when op
              (let ([volatile (keep
                      (lambda (c) (pair? (hashtable-ref deleted-by c '())))
                      (op-preconds op))])
                (unless (null? volatile)
                  (hashtable-set! snap-map action
                    (map (lambda (c)
                           (cons c (string-append "pre:" c)))
                         volatile)))))))
        plan)

      ;; 1. All initial conditions (with outlist for deleted ones)
      (for-each
        (lambda (c)
          (let ([deleters (hashtable-ref deleted-by c '())])
            (if (null? deleters)
                (network-add-node! net c c)
                (network-add-node! net c c
                  (list (cons 'justifications
                    (list (make-justification "SL" '() deleters "" ""))))))))
        initial-state)

      ;; 2. Execute plan steps in order
      (for-each
        (lambda (action)
          (let ([op (hashtable-ref idx action #f)])
            (when op
              (let* ([aid (string-append "do:" action)]
                     [snaps (hashtable-ref snap-map action '())])
                ;; 2a. Create snapshots for volatile preconditions
                (for-each
                  (lambda (pair)
                    (let ([snap-id (cdr pair)]
                          [cond-name (car pair)])
                      (unless (hashtable-ref (network-nodes net) snap-id #f)
                        (let ([creator (hashtable-ref created-by cond-name #f)])
                          (if creator
                              (network-add-node! net snap-id cond-name
                                (list (cons 'justifications
                                  (list (make-justification "SL"
                                          (list creator) '() "" "")))))
                              (network-add-node! net snap-id cond-name))))))
                  snaps)
                ;; 2b. Action node (snapshot substitution in preconds)
                (let ([preconds (map (lambda (c)
                                       (let ([s (assoc c snaps)])
                                         (if s (cdr s) c)))
                                     (op-preconds op))])
                  (network-add-node! net aid action
                    (list (cons 'justifications
                      (list (make-justification "SL" preconds '()
                              action ""))))))
                ;; 2c. Effect nodes (with outlist for later deletions)
                (for-each
                  (lambda (c)
                    (unless (hashtable-ref (network-nodes net) c #f)
                      (let ([del (hashtable-ref deleted-by c '())])
                        (network-add-node! net c c
                          (list (cons 'justifications
                            (list (make-justification "SL" (list aid) del
                                    "" ""))))))))
                  (op-add-list op))))))
        plan)

      ;; 3. Fix dependent registrations and truth values.
      ;; Outlist entries created before their target actions need
      ;; re-registration; truth values need recomputing for deletions.
      (network-rebuild-dependents! net)
      (network-recompute-all! net)

      net))

  ;;; Set helpers

  (define (subset? a b)
    (or (null? a) (and (member (car a) b) (subset? (cdr a) b))))

  (define (union a b)
    (let loop ([xs a] [r b])
      (if (null? xs) r
          (loop (cdr xs) (if (member (car xs) r) r (cons (car xs) r))))))

  (define (difference a b)
    (keep (lambda (x) (not (member x b))) a))

  (define (keep pred lst)
    (cond [(null? lst) '()]
          [(pred (car lst)) (cons (car lst) (keep pred (cdr lst)))]
          [else (keep pred (cdr lst))]))

) ;; end library
