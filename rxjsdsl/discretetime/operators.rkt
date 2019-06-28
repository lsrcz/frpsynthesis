#lang rosette

(require "dsl.rkt")
(require "instruction.rkt")

(provide (all-defined-out))

;; We want to be able to evaluate instructions in a standardized way,
;; but the RxJS operators have different signatures.
;; For each operator, we define a struct holding the name of the operator for printing
;; a function that calls the operator given an instruction and a list of past registers
;; and a function for pretty-printing

(struct operator
  (name call print printrx) #:transparent)

(define (call-insn op insn past-vars)
  ((operator-call op) insn past-vars))

(define (print-insn op insn varname past-vars)
    (format "  (define ~a (~a ~a))" varname (operator-name op) ((operator-print op) insn past-vars)))

(define (print-insn-rx op insn varname past-vars)
  (format "  var ~a = ~a;" varname ((operator-printrx op) insn past-vars)))

;; a list of higher-order int->int functions
(define unary-functions (list add1 sub1 (λ (i) 1) (λ (i) -1) (λ (i) #f) cadr))
(define unary-functions-str (list "add1" "sub1" "(λ (i) 1)" "(λ (i) -1)" "(λ (i) #f)" "cadr"))
(define unary-functions-str-rx (list "e => e + 1" "e => e - 1" "e => 1" "e => -1" "e => false" "lst => lst[1]"))
;; higher-order int->int->int functions
(define binary-functions (list + -))
(define binary-functions-str (list "+" "-"))
(define binary-functions-str-rx (list "(l, r) => l + r" "(l, r) => l - r"))

(define (print-operator-rx name func-list)
  (λ (reg-insn past-vars)
    (format "~a.pipe(~a(~a))" (get-input-stream1 reg-insn past-vars) name (get-argfunc reg-insn func-list))))

(define (print-operator-static-rx name)
  (λ (reg-insn past-vars)
    (format "~a(~a, ~a)" name (get-input-stream1 reg-insn past-vars) (get-input-stream2 reg-insn past-vars))))

(define (print-operator-stream-rx name)
  (λ (reg-insn past-vars)
    (format "~a.pipe(~a(~a))" (get-input-stream1 reg-insn past-vars) name (get-input-stream2 reg-insn past-vars))))

(define rxMap-op
  (operator "rxMap"
            (λ (reg-insn past-vars) (rxMap (get-argfunc reg-insn unary-functions)
                                           (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn unary-functions-str)
                                           (get-input-stream1 reg-insn past-vars)))
            (print-operator-rx "map" unary-functions-str-rx)
            ))

(define rxMerge-op
  (operator "rxMerge"
            (λ (reg-insn past-vars) (rxMerge (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            (print-operator-static-rx "merge")))

(define rxScan-op
  (operator "rxScan"
            (λ (reg-insn past-vars) (rxScan (get-argfunc reg-insn binary-functions)
                                            (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn binary-functions-str)
                                            (get-input-stream1 reg-insn past-vars)))
            (print-operator-rx "scan" binary-functions-str-rx)))

(define rxWithLatestFrom-op
  (operator "rxWithLatestFrom"
            (λ (reg-insn past-vars) (rxWithLatestFrom (get-input-stream1 reg-insn past-vars)
                                                      (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            (print-operator-stream-rx "withLatestFrom")))

(define rxFilter-op
  (operator "rxFilter"
            (λ (reg-insn past-vars) (rxFilter (get-argfunc reg-insn unary-functions)
                                              (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn unary-functions-str)
                                            (get-input-stream1 reg-insn past-vars)))
            (print-operator-rx "filter" unary-functions-str-rx)))

(define operator-list (list rxMap-op rxMerge-op rxScan-op rxWithLatestFrom-op rxFilter-op))
(define operator-id-lookup (make-hash (list (cons "rxMap" 0)
                                            (cons "rxMerge" 1)
                                            (cons "rxScan" 2)
                                            (cons "rxWithLatestFrom" 3)
                                            (cons "rxFilter" 4))))

