#lang rosette

(require "../model1/model.rkt")
(require "../model1/operators.rkt")
(require "../model1/sketch.rkt")

(define list-length 4)

(define sym-list1 (get-sym-list list-length))
(define sym-list2 (get-sym-list list-length))

(define (program1 lst1 lst2)
  (maximum-dc (zipwith-dc + (map-dc (λ (i) (+ i i i)) lst1) lst2)))

(define sk (get-symbolic-sketch 3 2))

(define evaled-sk ((get-sketch-function sk) sym-list1 sym-list2))

(define b (time (synthesize #:forall (symbolics (list sym-list1 sym-list2))
                            #:guarantee (assert (equal? evaled-sk (program1 sym-list1 sym-list2))))))

(if (unsat? b)
    (println "Synthesis from reference function is unsat")
    (print-sketch sk b))