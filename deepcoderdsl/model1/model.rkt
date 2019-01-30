#lang rosette

(provide (all-defined-out))

;; NB: it's possible to create distinct symbolic vars
;; which *don't* refer to same var but have same name
;; always get vars through these functions just so
;; naming is clear

(define (get-sym-bool)
  (define-symbolic* b boolean?)
  b)
(define (get-sym-int)
  (define-symbolic* i integer?)
  i)

;; symbolic arguments

(define (get-sym-list l)
  (for/list ([i (range l)])
    (get-sym-int)))

;; insn struct and helpers

(struct stream-insn 
  (op-index arg-index1 arg-index2 option-index arg-int) #:transparent)

(define (get-input-stream insn past-vars)
  (list-ref past-vars (stream-insn-arg-index1 insn)))

(define (get-input-stream2 insn past-vars)
  (list-ref past-vars (stream-insn-arg-index2 insn)))

(define (get-option-index insn)
  (stream-insn-option-index insn)) 

(define (get-integer-arg insn)
  (stream-insn-arg-int insn))

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* streamidx integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* option-index integer?)
  (define-symbolic* arg-int integer?)
  (stream-insn op streamidx arg2 option-index arg-int))

(define (get-holes-list count)
  (for/list ([i (range count)]) (get-insn-holes)))

(define (get-retval-idx)
  (define-symbolic* retval-idx integer?)
  retval-idx)

;;;; other helpers

(define  (same program1 program2 . inputs)
  (equal? (apply program1 inputs)
                  (apply program2 inputs)))