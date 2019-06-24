#lang racket

(require rackunit)

(require "instruction.rkt")
(require "operators.rkt")
(require "sketch.rkt")

;; stream.map(add1).scan(+)
(define sk1 (sketch (list (insn (hash-ref operator-id-lookup "rxMap") 0 9 0)
                          (insn (hash-ref operator-id-lookup "rxScan") 1 9 0))
                    2
                    1))

(check-equal? (operator-lookup sk1 0) rxMap-op)

(check-equal? ((get-sketch-function sk1) '(0 1 2)) '(1 3 6))
