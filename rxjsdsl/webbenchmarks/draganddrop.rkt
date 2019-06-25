#lang racket

(require "../discretetime/dsl.rkt")
(require "../discretetime/sketch.rkt")
(require "../discretetime/model.rkt")

(define (target-function up move down)
  ;(rxFilter (lambda (lst) (not (eq? (cadr lst) #f)))
            (rxWithLatestFrom
             move
             (rxMerge
              (rxMap (lambda (e) #f) up)
              down)));)

(target-function (list NOEVENT NOEVENT NOEVENT NOEVENT 4 NOEVENT NOEVENT NOEVENT 5 NOEVENT NOEVENT NOEVENT)
                 (list 1 2 NOEVENT 4 NOEVENT 6 NOEVENT 8 NOEVENT 10 NOEVENT 12)
                 (list NOEVENT NOEVENT 1 NOEVENT NOEVENT NOEVENT 2 NOEVENT NOEVENT NOEVENT 3 NOEVENT))
                 