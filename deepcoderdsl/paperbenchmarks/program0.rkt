#lang rosette

(require "../model1/model.rkt")
(require "../model1/operators.rkt")
(require "../model1/sketch.rkt")

(define list-length 4)

(define sym-list (get-sym-list list-length))
(define sym-int (get-sym-int))

(define (program0 k b)
  (sum-dc (take-dc k (sort-dc b))))

(define sk (get-symbolic-sketch 3 2))

(define evaled-sk ((get-sketch-function sk) sym-int sym-list))

(define b (time (synthesize #:forall (symbolics (list sym-int sym-list))
                            #:guarantee (assert (equal? evaled-sk (program0 sym-int sym-list))))))

(if (unsat? b)
    (println "Synthesis from reference function is unsat")
    (print-sketch sk b))




  
