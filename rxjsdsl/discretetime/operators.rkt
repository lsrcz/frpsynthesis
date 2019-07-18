#lang rosette

(require "dsl.rkt")
(require "instruction.rkt")
(require "model.rkt")
(require "constraints.rkt")

(provide (all-defined-out))

;; We want to be able to evaluate instructions in a standardized way,
;; but the RxJS operators have different signatures.
;; For each operator, we define a struct holding the name of the operator for printing
;; a function that calls the operator given an instruction and a list of past registers
;; and a function for pretty-printing

(struct operator
  (name call print printrx typecheck constraint) #:transparent)

(define (call-insn insn past-vars)
  ((operator-call (get-operator insn operator-list)) insn past-vars))

(define (constraint-insn insn past-vars)
  ((operator-constraint (get-operator insn operator-list)) insn past-vars))

(define (check-insn-type insn past-tys)
  ((operator-typecheck (get-operator insn operator-list)) insn past-tys))

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
(define constants (list 480 10 37 39 0 -1 1))

(define (set-unary-functions funcs strs rxstrs)
  (set! unary-functions funcs)
  (set! unary-functions-str strs)
  (set! unary-functions-str-rx rxstrs))
(define (set-binary-functions funcs strs rxstrs)
  (set! binary-functions funcs)
  (set! binary-functions-str strs)
  (set! binary-functions-str-rx rxstrs))
(define (set-constants constantslst)
  (set! constants constantslst))



(define (print-operator-rx name func-list)
  (λ (reg-insn past-vars)
    (format "~a.pipe(~a(~a))" (get-input-stream1 reg-insn past-vars) name (get-argfunc reg-insn func-list))))

(define (print-operator-static-rx name)
  (λ (reg-insn past-vars)
    (format "~a(~a, ~a)" name (get-input-stream1 reg-insn past-vars) (get-input-stream2 reg-insn past-vars))))

(define (print-operator-stream-rx name)
  (λ (reg-insn past-vars)
    (format "~a.pipe(~a(~a))" (get-input-stream1 reg-insn past-vars) name (get-input-stream2 reg-insn past-vars))))

(define (no-constraint)
  (lambda (reg-insn past-vars) #t))

(define (distinct-constraint2)
  (lambda (reg-insn past-vars) (less-than-one-event (get-input-stream1 reg-insn past-vars)
                                                       (get-input-stream2 reg-insn past-vars))))

(define rxMap-op
  (operator "rxMap"
            (λ (reg-insn past-vars) (rxMap (get-argfunc reg-insn unary-functions)
                                           (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn unary-functions-str)
                                           (get-input-stream1 reg-insn past-vars)))
            (print-operator-rx "map" unary-functions-str-rx)
            'not-implemented
            (no-constraint)
            ))

(define rxMerge-op
  (operator "rxMerge"
            (λ (reg-insn past-vars) (rxMerge (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            (print-operator-static-rx "merge")
            'not-implemented
            (distinct-constraint2)
            ))

(define rxScan-op
  (operator "rxScan"
            (λ (reg-insn past-vars) (rxScan (get-argfunc reg-insn binary-functions)
                                            (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn binary-functions-str)
                                            (get-input-stream1 reg-insn past-vars)))
            (print-operator-rx "scan" binary-functions-str-rx)
            'not-implemented
            (no-constraint)
            ))

(define rxScanWithInit-op
  (operator "rxScanWithInit"
            (lambda (reg-insn past-vars) (rxScanWithInit
                                          (get-argfunc reg-insn binary-functions)
                                          (get-input-stream1 reg-insn past-vars)
                                          (get-constant reg-insn constants)))
            (lambda (reg-insn past-vars) (format "~a ~a ~a"
                                                 (get-argfunc reg-insn binary-functions-str)
                                                 (get-input-stream1 reg-insn past-vars)
                                                 (get-constant reg-insn constants)
                                                 ))
            'not-implemented
            'not-implemented
            (no-constraint)))

(define rxWithLatestFrom-op
  (operator "rxWithLatestFrom"
            (λ (reg-insn past-vars) (rxWithLatestFrom (get-input-stream1 reg-insn past-vars)
                                                      (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            (print-operator-stream-rx "withLatestFrom")
            'not-implemented
            (distinct-constraint2)
            ))

(define rxFilter-op
  (operator "rxFilter"
            (λ (reg-insn past-vars) (rxFilter (get-argfunc reg-insn unary-functions)
                                              (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn unary-functions-str)
                                            (get-input-stream1 reg-insn past-vars)))
            (print-operator-rx "filter" unary-functions-str-rx)
            'not-implemented
            (no-constraint)
            ))

(define sync-constant-op
  (operator "sync-constant"
            (λ (reg-insn past-vars) (sync-constant (get-constant reg-insn constants)
                                                   (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a"
                                            (get-constant reg-insn constants)))
            'not-implemented
            (λ (reg-insn past-tys) 'int)
            'not-implemented))

(define (check-type-one-stream ty1 ret)
  (λ (reg-insn past-tys)
    (if (eq? (get-input-stream1 reg-insn past-tys) ty1)
        ret
        'bad)))

(define (check-type-two-stream ty1 ty2 ret)
  (λ (reg-insn past-tys)
    (if (and (eq? (get-input-stream1 reg-insn past-tys) ty1) (eq? (get-input-stream2 reg-insn past-tys) ty2))
        ret
        'bad)))

(define (check-type-three-stream ty1 ty2 ty3 ret)
  (λ (reg-insn past-tys)
    (if (and (eq? (get-input-stream1 reg-insn past-tys) ty1) (eq? (get-input-stream2 reg-insn past-tys) ty2) (eq? (get-input-stream3 reg-insn past-tys) ty3))
        ret
        'bad)))

(define sync-lessThan-op
  (operator "sync-lessThan"
            (λ (reg-insn past-vars) (sync-lessThan (get-input-stream1 reg-insn past-vars)
                                                   (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            (check-type-two-stream 'int 'int 'bool)
            'not-implemented))

(define sync-equalTo-op
  (operator "sync-equalTo"
            (λ (reg-insn past-vars) (sync-equalTo (get-input-stream1 reg-insn past-vars)
                                                   (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            (check-type-two-stream 'int 'int 'bool)
            'not-implemented))

(define sync-lessThanOrEqualTo-op
  (operator "sync-lessThanOrEqualTo"
            (λ (reg-insn past-vars) (sync-lessThanOrEqualTo (get-input-stream1 reg-insn past-vars)
                                                   (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            (check-type-two-stream 'int 'int 'bool)
            'not-implemented))

(define sync-and-op
  (operator "sync-and"
            (λ (reg-insn past-vars) (sync-and (get-input-stream1 reg-insn past-vars)
                                              (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            (check-type-two-stream 'bool 'bool 'bool)
            'not-implemented))

(define sync-or-op
  (operator "sync-or"
            (λ (reg-insn past-vars) (sync-or (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            (check-type-two-stream 'bool 'bool 'bool)
            'not-implemented))

(define sync-not-op
  (operator "sync-not"
            (λ (reg-insn past-vars) (sync-not (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a"
                                            (get-input-stream1 reg-insn past-vars)))
            'not-implemented
            (check-type-one-stream 'bool 'bool)
            'not-implemented))

(define sync-if-op
  (operator "sync-if"
            (λ (reg-insn past-vars) (sync-if (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)
                                             (get-input-stream3 reg-insn past-vars)
                                             ))
            (λ (reg-insn past-vars) (format "~a ~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)
                                            (get-input-stream3 reg-insn past-vars)))
            'not-implemented
            (check-type-three-stream 'bool 'int 'int 'int)
            'not-implemented))

(define sync-add-op
  (operator "sync-add"
            (λ (reg-insn past-vars) (sync-add (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            (check-type-two-stream 'int 'int 'int)
            'not-implemented))

(define sync-sub-op
  (operator "sync-sub"
            (λ (reg-insn past-vars) (sync-sub (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            (check-type-two-stream 'int 'int 'int)
            'not-implemented))

(define sync-mul-op
  (operator "sync-mul"
            (λ (reg-insn past-vars) (sync-mul (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            (check-type-two-stream 'int 'int 'int)
            'not-implemented))

(define sync-map2-op
  (operator "sync-map2"
            (λ (reg-insn past-vars) (sync-map2 (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))
            'not-implemented
            'not-implemented
            'not-implemented))


(define operator-list (list rxMap-op rxMerge-op rxScan-op rxWithLatestFrom-op rxFilter-op rxScanWithInit-op))
(define operator-id-lookup (make-hash (list (cons "rxMap" 0)
                                            (cons "rxMerge" 1)
                                            (cons "rxScan" 2)
                                            (cons "rxWithLatestFrom" 3)
                                            (cons "rxFilter" 4)
                                            (cons "rxScanWithInit" 5))))

#|
(define operator-list
  (list
   sync-add-op
   sync-sub-op
   sync-mul-op
   sync-constant-op
  

   sync-and-op
   sync-or-op
   sync-not-op
   sync-if-op
 sync-lessThan-op
    sync-equalTo-op
   sync-lessThanOrEqualTo-op

   sync-map2-op))
(define operator-id-lookup (make-hash (list (cons "sync-add-op" 0)
                                            (cons "sync-sub-op" 1)
                                            (cons "sync-mul-op" 2)
                                            (cons "sync-constant" 3)
                                            
                                            (cons "sync-and-op" 4)
                                            (cons "sync-or-op" 5)
                                            (cons "sync-not-op" 6)
                                            (cons "sync-if-op" 7)
                                            (cons "sync-lessThan" 8)
                                            
                                            (cons "sync-equalTo" 9)
                                            (cons "sync-lessThanOrEqualTo" 10)

                                            (cons "sync-map2-op" 11))))|#
