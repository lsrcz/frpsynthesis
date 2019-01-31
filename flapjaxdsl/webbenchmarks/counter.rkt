#lang rosette

(require "../model1/model.rkt")
(require "../model1/operators.rkt")
(require "../model1/sketch.rkt")

(define stream-length 4)

(define sym-inc (new-event-stream get-sym-int stream-length))
(define sym-dec (new-event-stream get-sym-int stream-length))

(define (counter-func inc dec)
  (startsWith 0 (collectE 0 + (mergeE (constantE 1 inc) (constantE -1 dec)))))

(define sk (get-symbolic-sketch 5 2))

(define evaled-sk ((get-sketch-function sk) sym-inc sym-dec))

(define b (time (synthesize #:forall (symbolics (list sym-inc sym-dec))
                            #:guarantee (assert (equal? evaled-sk (counter-func sym-inc sym-dec))))))

(if (unsat? b)
    (println "Synthesis from reference function: unsat")
    (print-sketch sk b))