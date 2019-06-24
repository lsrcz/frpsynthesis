#lang racket

(require rackunit)

(require "model.rkt")
(require "dsl.rkt")

(check-equal? (rxMap add1 (list 0 1 NOEVENT 2)) (list 1 2 NOEVENT 3))

(check-equal? (rxMerge (list 0 NOEVENT 2 NOEVENT) (list NOEVENT 1 NOEVENT 3)) (list 0 1 2 3))
(check-equal? (rxMerge (list NOEVENT 2 4) (list 1 3 NOEVENT)) (list 1 2 4))

(check-equal? (rxScan + (list 0 1 2 3)) (list 0 1 3 6))
(check-equal? (rxScan + '()) '())
(check-equal? (rxScan + (list NOEVENT 1)) (list NOEVENT 1))
(check-equal? (rxScan + (list NOEVENT 1 NOEVENT 1 NOEVENT 2)) (list NOEVENT 1 NOEVENT 2 NOEVENT 4))