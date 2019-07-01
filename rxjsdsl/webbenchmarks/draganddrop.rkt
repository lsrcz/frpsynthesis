#lang rosette

(require "../discretetime/dsl.rkt")
(require "../discretetime/sketch.rkt")
(require "../discretetime/model.rkt")
(require "../discretetime/correctloop.rkt")
(require "../discretetime/disambiguate.rkt")

(define stream-length 17)

;(define get-sym-int get-sym-abstract)

(define sym-up (new-event-stream get-sym-int stream-length))
(define sym-move (new-event-stream get-sym-int stream-length))
(define sym-down (new-event-stream get-sym-int stream-length))

(define input1-up (list NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT 4
                        NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT 5
                        NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT))
(define input1-move (list 1       2       NOEVENT 4       5       NOEVENT
                          7       8       NOEVENT 9       10      NOEVENT
                          12      13      NOEVENT 14      15))
(define input1-down (list NOEVENT NOEVENT 1       NOEVENT NOEVENT NOEVENT
                          NOEVENT NOEVENT 2       NOEVENT NOEVENT NOEVENT
                          NOEVENT NOEVENT 3       NOEVENT NOEVENT))
(define concrete-output (list NOEVENT NOEVENT NOEVENT (list 4 1) (list 5 1)
                              NOEVENT NOEVENT NOEVENT NOEVENT (list 9 2) (list 10 2)
                              NOEVENT NOEVENT NOEVENT NOEVENT (list 14 3) (list 15 3)))

(define (target-function up move down)
  (rxFilter cadr
            (rxWithLatestFrom
             move
             (rxMerge
              (rxMap (lambda (e) #f) up)
              down))))

(define sym-sk (make-symbolic-sketch 4 3))
(define evaled-sk ((get-sketch-function sym-sk) sym-up sym-move sym-down))

#|
(define b (time (synthesize #:forall (symbolics (list sym-up sym-move sym-down))
                            #:guarantee (assert (equal? evaled-sk (target-function sym-up sym-move sym-down))))))

(if (unsat? b)
    (println "Synthesis from reference function: unsat")
    (begin
      (print-sketch sym-sk b)
      (print-sketch-rx sym-sk b)))

(equal? (target-function input1-up input1-move input1-down) concrete-output)

(define b1 (time (synthesize #:forall (list)
                 #:guarantee (assert (equal?
  concrete-output
  ((get-sketch-function sym-sk) input1-up input1-move input1-down))))))
                          
(if (unsat? b1)
    (println "Synthesis from trace: unsat")
    (begin
      (print-sketch sym-sk b1)
      (print-sketch-rx sym-sk b1)))|#

;; loop

(define t (gen-trace input1-up input1-move input1-down))

(synthesize-by-disambiguate sym-sk t 15 (list get-sym-int get-sym-int get-sym-int))

#|
(target-function (list NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT 4       NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT 5       NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT)
                 (list 1       2       NOEVENT 4       5       NOEVENT 7       8       NOEVENT 9       10      NOEVENT 12      13      NOEVENT 14      15)
                 (list NOEVENT NOEVENT 1       NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT 2       NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT 3       NOEVENT NOEVENT))



(define stream-length 4)

(define sym-inc (new-event-stream get-sym-int stream-length))
(define sym-dec (new-event-stream get-sym-int stream-length))

(define (target-function inc dec) (rxScan + (rxMerge (rxMap (λ (i) 1) inc)
                                                     (rxMap (λ (i) -1) dec))))

(define sym-sk (make-symbolic-sketch 4 2))

(define evaled-sk ((get-sketch-function sym-sk) sym-inc sym-dec))

(define b (time (synthesize #:forall (symbolics (list sym-inc sym-dec))
                            #:guarantee (assert (equal? evaled-sk (target-function sym-inc sym-dec))))))

(if (unsat? b)
    (println "Synthesis from reference function: unsat")
    (begin
      (print-sketch sym-sk b)
      (print-sketch-rx sym-sk b)))|#