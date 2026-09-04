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

  ;;; Phase 2: TMS execution. Build a belief network that explains
  ;;; WHY each goal is achieved — the justification chain from goals
  ;;; back through operators to initial conditions.
  ;;;
  ;;; The TMS adds: network-explain traces, retraction what-if analysis,
  ;;; network-trace-assumptions to find critical initial conditions.

  (define (gps->network initial-state plan ops)
    (let ([net (make-network)]
          [idx (make-hashtable string-hash string=?)])
      (for-each (lambda (o) (hashtable-set! idx (op-action o) o)) ops)
      ;; Initial conditions as premise nodes
      (for-each (lambda (c) (network-add-node! net c c)) initial-state)
      ;; Each plan step: action node justified by preconditions,
      ;; effect nodes justified by the action
      (for-each
        (lambda (action)
          (let ([op (hashtable-ref idx action #f)])
            (when op
              (let ([aid (string-append "do:" action)])
                (network-add-node! net aid action
                  (list (cons 'justifications
                    (list (make-justification "SL" (op-preconds op) '()
                            action "")))))
                (for-each
                  (lambda (c)
                    (unless (hashtable-ref (network-nodes net) c #f)
                      (network-add-node! net c c
                        (list (cons 'justifications
                          (list (make-justification "SL" (list aid) '()
                                  "" "")))))))
                  (op-add-list op))))))
        plan)
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
