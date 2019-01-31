#lang rosette
(provide (all-defined-out))

(require "model.rkt")

(struct operator
  (name call print) #:transparent)

(define (call-stream-insn op insn past-vars)
  ((operator-call op) insn past-vars))

(define (print-stream-insn op insn varname past-vars)
    (format "  (define ~a (~a ~a))" varname (operator-name op) ((operator-print op) insn past-vars)))

;; oneE

;; zeroE

;; mapE

(define (mapE proc evt-stream)
  (map (λ (e) (if (empty-event? e) e (proc e))) evt-stream))

(define mapE-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (curry (list-ref (append
                                                        inttointfuncs
                                                        inttoboolfuncs) (get-option-index insn))
                                             (get-integer-arg insn))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref (append inttointfuncs-string inttoboolfuncs-string)
                                                          (get-option-index insn))
                                                (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))))

;; mergeE

(define (mergeE evt-stream1 evt-stream2)
  (map (λ (evt1 evt2) (if (empty-event? evt2) evt1 evt2))
       evt-stream1 evt-stream2))

(define mergeE-op
  (operator "mergeE"
            (λ (insn past-vars) (mergeE (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))))

;; switchE

;; condE

;; filterE

(define (filterE pred stream)
  (map (λ (e) (if (and (not-empty-event? e) (pred e)) e 'no-evt)) stream))

(define filterE-op
  (operator "filterE"
            (λ (insn past-vars) (filterE (list-ref genericfuncs (get-option-index insn))
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref genericfuncs-string (get-option-index insn))
                                        (get-input-stream insn past-vars)))))

(define filterE-imm-op
  (operator "filterE"
            (λ (insn past-vars) (filterE (curry (list-ref inttoboolfuncs (get-option-index insn))
                                                (get-integer-arg insn))
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolfuncs-string (get-option-index insn))
                                                (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))))

;; ifE

(define (ifE guard-stream true-stream false-stream)
  (map (λ (guard true false) (if (empty-event? guard) 'no-evt (if guard true false)))
       guard-stream true-stream false-stream))

(define ifE-op
  (operator "ifE"
            (λ (insn past-vars) (ifE (get-input-stream insn past-vars)
                                     (get-input-stream2 insn past-vars)
                                     (get-input-stream3 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)
                                        (get-input-stream3 insn past-vars)))))

;; constantE

(define (constantE const evt-stream)
  (map (λ (x) (if (empty-event? x) 'no-evt const)) evt-stream))

(define constantE-imm-op
  (operator "constantE"
            (λ (insn past-vars) (constantE (get-integer-arg insn)
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (stream-insn-arg-int insn)
                                        (get-input-stream insn past-vars)))))
(define constantE-op
  (operator "constantE"
            (λ (insn past-vars) (constantE (list-ref constantB-consts (get-option-index insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (get-option-index insn))
                                        (get-input-stream insn past-vars)))))

;; collectE

(define (collectE init proc lst)
  (for/list ([i (range (length lst))])
    (if (empty-event? (list-ref lst i))
        'no-evt
        (foldl (λ (n m) (if (empty-event? n) m (proc n m))) init (take lst (add1 i))))))

(define collectE-imm-op
  (operator "collectE"
            (λ (insn past-vars) (collectE (get-integer-arg insn) (list-ref
                                                                  inttointtointfuncs
                                                                  (get-option-index insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn) (list-ref
                                                                  inttointtointfuncs-string
                                                                  (get-option-index insn))
                                          (get-input-stream insn past-vars)))))

;; andE

(define (andE evt-stream1 evt-stream2)
  (map (λ (e1 e2) (if (and (not (empty-event? e1))
                           (not (empty-event? e2))
                           (and e1 e2))
                      #t
                      'no-evt)) evt-stream1 evt-stream2))

(define andE-op
  (operator "andE"
            (λ (insn past-vars) (andE (get-input-stream insn past-vars)
                                      (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

;; orE

(define (orE evt-stream1 evt-stream2)
  (map (λ (e1 e2) (if (and (not (empty-event? e1))
                           (not (empty-event? e2))
                           (or e1 e2))
                      #t
                      'no-evt)) evt-stream1 evt-stream2))

(define orE-op
  (operator "orE"
            (λ (insn past-vars) (orE (get-input-stream insn past-vars)
                                     (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

;; notE

(define (notE evt-stream)
  (map (λ (e) (if (empty-event? e) e (not e))) evt-stream))

(define notE-op
  (operator "notE"
            (λ (insn past-vars) (notE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; filterRepeatsE

(define (filterRepeatsE evt-stream)
  (letrec ([f (λ (evt rest)
                (cond [(empty? rest) evt]
                      [(equal? evt (first rest)) 'no-evt]
                      [(not-empty-event? (first rest)) evt]
                      [else (f evt (cdr rest))]))])
    (for/list ([i (range 1 (add1 (length evt-stream)))])
      (let ([lst (take evt-stream i)])
        (if (empty-event? (last lst))
            'no-evt
            (f (last lst) (cdr (reverse lst))))))))

(define filterRepeatsE-op
  (operator "filterRepeatsE"
            (λ (insn past-vars) (filterRepeatsE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; snapshotE

(define (snapshotE evt-stream behavior1)
  (map (λ (e b) (if (not-empty-event? e) b e)) evt-stream (behavior-changes behavior1)))

(define snapshotE-op
  (operator "snapshotE"
            (λ (insn past-vars) (snapshotE (get-input-stream insn past-vars)
                                           (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

;; onceE

;; skipFirstE

;; delayE

(define (delayE interval evt-stream)
 ; (append
   (for/list ([i (range (length evt-stream))])
    (if (> interval i)
        'no-evt
        (list-ref evt-stream (- i interval))))
         ; (list-tail evt-stream (- (length evt-stream) interval))))
  )

(define delayE-op
  (operator "delayE"
            (λ (insn past-vars) (delayE (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))

;; blindE

(define (blindE interval evt-stream)
  (letrec ([f (λ (evts wait-time)
                (cond [(empty? evts) '()]
                      [(and (>= 0 wait-time) (not-empty-event? (first evts))) (cons (first evts) (f (rest evts) interval))]
                      [else (cons 'no-evt (f (rest evts) (sub1 wait-time)))]))])
    (f evt-stream 0)))

;; calmE

(define (calmE interval evt-stream)
  (letrec ([f (λ (evts last-sent buffered-evt)
                (cond [(and (empty? evts) (empty-event? buffered-evt)) '()]
                      [(and (empty? evts) (>= last-sent interval)) (list buffered-evt)]
                      [(empty? evts) (cons 'no-evt (f evts (add1 last-sent) buffered-evt))]
                      [else (let ([output (if (>= last-sent interval) buffered-evt 'no-evt)]
                                  [new-buffer (if (not-empty-event? (first evts)) (first evts) buffered-evt)]
                                  [new-last-sent (cond [(and (not-empty-event? (first evts))
                                                          (>= last-sent interval)) 0]
                                                       [(< last-sent interval) (add1 last-sent)]
                                                       [else last-sent])])
                              (cons output (f (rest evts) new-last-sent new-buffer)))]))])
    (f evt-stream 0 'no-evt)))

;; timerE

;;;;;;;;;;;;;;;;;;; behavior operators ;;;;;;;;;;;;;;;;;

;; startsWith

(define (startsWith init-value evt-stream)
  (behavior init-value (for/list ([i (range (length evt-stream))])
                         (findf (λ (e) (not (empty-event? e)))
                                (reverse (cons init-value (take evt-stream (add1 i))))))))

(define startsWith-imm-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn)
                                        (get-input-stream insn past-vars)))))
(define startsWith-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (list-ref constantB-consts (get-option-index insn))
                                            (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (get-option-index insn))
                                        (get-input-stream insn past-vars)))))

;; changes

(define (changes behaviorB)
    (filterRepeatsE (behavior-changes behaviorB)))

(define changes-op
  (operator "changes"
            (λ (insn past-vars) (changes (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; constantB

(define (constantB const behaviorB)
  (behavior const (constantE const (changes behaviorB))))

(define constantB-imm-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))

(define constantB-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (list-ref constantB-consts (get-option-index insn)) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (get-option-index insn))
                                        (get-input-stream insn past-vars)))))

;; delayB

(define (delayB interval behavior1)
  (behavior (behavior-init behavior1) (delayE interval (behavior-changes behavior1))))

(define delayB-op
  (operator "delayB"
            (λ (insn past-vars) (delayB (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))

;; switchB

;; andB

(define (andB behavior1 behavior2)
  (behavior (and (behavior-init behavior1) (behavior-init behavior2))
            (map (λ (b1 b2) (and b1 b2)) (behavior-changes behavior1) (behavior-changes behavior2))))

(define andB-op
  (operator "andB"
            (λ (insn past-vars) (andB (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))))

;; orB

(define (orB behavior1 behavior2)
  (behavior (or (behavior-init behavior1) (behavior-init behavior2))
            (map (λ (b1 b2) (or b1 b2)) (behavior-changes behavior1) (behavior-changes behavior2))))

(define orB-op
  (operator "orB"
            (λ (insn past-vars) (orB (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))))

;; notB

(define (notB behavior1)
  (behavior (not (behavior-init behavior1)) (notE (behavior-changes behavior1))))

(define notB-op
  (operator "notB"
            (λ (insn past-vars) (notB (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; liftB

(define (liftB proc argB)
  (behavior (proc (behavior-init argB)) (map proc (behavior-changes argB))))

(define liftB-op
  (operator "liftB"
            (λ (insn past-vars) (liftB (curry (list-ref (append inttointfuncs inttoboolfuncs) (get-option-index insn)) (get-integer-arg insn)) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref (append inttointfuncs-string inttoboolfuncs-string) (get-option-index insn)) (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))))

;; condB

;; ifB

(define (ifB conditionB trueB falseB)
  (behavior (if (behavior-init conditionB) (behavior-init trueB) (behavior-init falseB))
            (map (λ (cB tB fB) (if cB tB fB))
                 (behavior-changes conditionB)
                 (behavior-changes trueB)
                 (behavior-changes falseB))))

(define ifB-op
  (operator "ifB"
            (λ (insn past-vars) (ifB (get-input-stream insn past-vars)
                                     (get-input-stream2 insn past-vars)
                                     (get-input-stream3 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)
                                        (get-input-stream3 insn past-vars)))))

;; timerB

;; blindB

;; calmB

;; collectB

(define (collectB init-val proc b1)
  (let ([b-init (proc init-val (behavior-init b1))])
         (letrec ([collect (λ (lst prev)
                       (if (empty? lst)
                           '()
                           (cons (proc (first lst) prev) (collect (rest lst) (proc (first lst) prev)))))])
           (behavior b-init (collect (behavior-changes b1) b-init)))))

(define collectB-imm-op
  (operator "collectB"
            (λ (insn past-vars) (collectB (get-integer-arg insn) (list-ref
                                                                  inttointtointfuncs
                                                                  (get-option-index insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (list-ref
                                        inttointtointfuncs-string
                                         (get-option-index insn))
                                        (get-input-stream insn past-vars)))))

;;;;;; higher-order functions ;;;;;;;;

;; ? -> ?
(define genericfuncs (list identity
                           identity
                           identity
                           identity
                           identity))

(define genericfuncs-string (list "(λ (e) e)"
                                  "(λ (e) e)"
                                  "(λ (e) e)"
                                  "(λ (e) e)"
                                  "(λ (e) e)"))

;; int -> int
(define inttointfuncs (list (λ (placeholder i) (+ i placeholder))
                            (λ (placeholder i) (- i placeholder))
                            (λ (placeholder i) (- placeholder i))
                            (λ (placeholder i) (+ i placeholder))
                            (λ (placeholder i) (+ i placeholder))
                           ; (λ (placeholder i) (* i placeholder))
                           ; (λ (i) (/ i placeholder)) ;; leave out division?
                           ; (λ (i) (/ placeholder i))
                            ))
(define inttointfuncs-string (list "(λ (i) (+ i ~a))"
                                   "(λ (i) (- i ~a))"
                                   "(λ (i) (- ~a i))"
                                   "(λ (i) (+ i ~a))"
                                   "(λ (i) (+ i ~a))"
                                  ; "(λ (i) (* i ~a))"
                                   ))
;; int -> bool
(define inttoboolfuncs (list (λ (placeholder i) (<= i placeholder))
                             (λ (placeholder i) (>= i placeholder))
                             (λ (placeholder i) (< i placeholder))
                             (λ (placeholder i) (> i placeholder))
                             (λ (placeholder i) (= i placeholder))
                             ))

(define inttoboolfuncs-string (list "(λ (i) (<= i ~a))"
                                    "(λ (i) (>= i ~a))"
                                    "(λ (i) (< i ~a))"
                                    "(λ (i) (> i ~a))"
                                    "(λ (i) (= i ~a))"
                                    ))

;; int -> int -> int
(define inttointtointfuncs (list +
                                 -
                                 ; *
                                 ; /
                                 min
                                 max
                                 +
                                 ))
(define inttointtointfuncs-string (list "+"
                                         "-"
                                         ; "*"
                                         ; "/"
                                         "min"
                                         "max"
                                         "+"
                                         ))

(define constantB-consts (list 'on 'off #t #f 'test))

(define operator-list
  (list ;; oneE-op
   ;; zeroE-op
   mapE-op
   mergeE-op
   ;; switchE-op
   ;; condE-op
   filterE-op
   filterE-imm-op
   ifE-op
   constantE-op
   constantE-imm-op
   collectE-imm-op
   andE-op
   orE-op
   notE-op
   filterRepeatsE-op
   snapshotE-op
   ;; onceE-op
   ;; skipFirstE-op
   delayE-op
   ;; blindE-op
   ;; calmE-op
   startsWith-op
   startsWith-imm-op
   changes-op
   constantB-op
   constantB-imm-op
   delayB-op
   ;; switchB-op
   andB-op
   orB-op
   notB-op
   liftB-op
   ;; condB-op
   ifB-op
   ;; timerB-op
   ;; blindB-op
   ;; calmB-op
   ))

(define (get-operator op-code)
  (list-ref operator-list op-code))