#lang racket

(require rackunit)

(require "model.rkt")
(require "dsl.rkt")

(check-equal? (rxCombineLatest + (list 1 NOEVENT 2 NOEVENT 4 NOEVENT NOEVENT)
                                 (list NOEVENT 1 NOEVENT NOEVENT NOEVENT 2 3))
              (list NOEVENT 2 3 NOEVENT 5 6 7))

(check-equal? (rxDistinct (list NOEVENT 1 1 NOEVENT 2 NOEVENT 2)) (list NOEVENT 1 NOEVENT NOEVENT 2 NOEVENT NOEVENT))

(check-equal? (rxFilter even? (list 0 2 NOEVENT 3 4 NOEVENT)) (list 0 2 NOEVENT NOEVENT 4 NOEVENT))

(check-equal? (rxMap add1 (list 0 1 NOEVENT 2)) (list 1 2 NOEVENT 3))

(check-equal? (rxMerge (list 0 NOEVENT 2 NOEVENT) (list NOEVENT 1 NOEVENT 3)) (list 0 1 2 3))
(check-equal? (rxMerge (list NOEVENT 2 4) (list 1 3 NOEVENT)) (list 1 2 4))

(check-equal? (rxPairwise (list 1 NOEVENT 3)) (list NOEVENT NOEVENT (cons 1 3)))
(check-equal? (rxPairwise (list NOEVENT 2 NOEVENT)) (list NOEVENT NOEVENT NOEVENT))
(check-equal? (rxPairwise (list NOEVENT NOEVENT 3)) (list NOEVENT NOEVENT NOEVENT))
(check-equal? (rxPairwise (list NOEVENT 2 3)) (list NOEVENT NOEVENT (cons 2 3)))

(check-equal? (rxScan + (list 0 1 2 3)) (list 0 1 3 6))
(check-equal? (rxScan + '()) '())
(check-equal? (rxScan + (list NOEVENT 1)) (list NOEVENT 1))
(check-equal? (rxScan + (list NOEVENT 1 NOEVENT 1 NOEVENT 2)) (list NOEVENT 1 NOEVENT 2 NOEVENT 4))

(check-equal? (rxSkip 3 (list NOEVENT 1 NOEVENT 1 1 NOEVENT 3 2)) (list NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT NOEVENT 3 2))

(check-equal? (rxTake 3 (list NOEVENT 1 NOEVENT 1 1 NOEVENT 3 2)) (list NOEVENT 1 NOEVENT 1 1 NOEVENT NOEVENT NOEVENT))