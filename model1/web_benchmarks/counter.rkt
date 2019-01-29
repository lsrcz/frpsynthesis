#lang rosette
(require "../model.rkt")
(require "../fj-operators.rkt")
(require "../sketch.rkt")

(define stream-length 3)

(define sym-inc (new-event-stream get-sym-int stream-length))
(define sym-dec (new-event-stream get-sym-int stream-length))

(define (ref-impl inc dec)
  (collectE 0 + (mergeE (constantE 1 inc) (constantE -1 dec))))

(define sk (get-symbolic-sketch 4 2))

(define evaled-sk ((get-sketch-function sk) sym-inc sym-dec))

(define b (time (synthesize #:forall (symbolics (list sym-inc sym-dec))
                            #:guarantee (assert (equal? evaled-sk (ref-impl sym-inc sym-dec))))))
                


