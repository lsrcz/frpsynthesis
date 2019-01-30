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

;; event streams are a list of events; each entry in the list represents a single time step
;; an event is a symbolic union over a symbol represents no event and a symbolic value

(define NOEVENT 'no-evt)

(define (empty-event? e)
  (eq? NOEVENT e))

(define (not-empty-event? e)
  (not (eq? NOEVENT e)))

(define (new-event-stream constructor n)
  (for/list ([i n])
    (if (get-sym-bool)
        (constructor)
        NOEVENT)))

;; behaviors are a struct containing an initial value and a list of (symbolic) values
;; each entry in the list represents a single timestep
;; no entry in that list can be a non event; behaviors have a value at every timestep

(struct behavior (init changes) #:transparent)

(define (new-behavior constructor n)
  (behavior (constructor) (for/list ([i n])
                            (constructor))))


;; insn struct and helpers

(struct stream-insn 
  (op-index arg-index1 arg-index2 arg-index3 option-index arg-int arg-int2) #:transparent)

(define (get-input-stream insn past-vars)
  (list-ref past-vars (stream-insn-arg-index1 insn)))

(define (get-input-stream2 insn past-vars)
  (list-ref past-vars (stream-insn-arg-index2 insn)))

(define (get-input-stream3 insn past-vars)
  (list-ref past-vars (stream-insn-arg-index3 insn)))

(define (get-option-index insn)
  (stream-insn-option-index insn)) 

(define (get-integer-arg insn)
  (stream-insn-arg-int insn))

(define (get-integer-arg2 insn)
  (stream-insn-arg-int2 insn))

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* streamidx integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (define-symbolic* option-index integer?)
  (define-symbolic* arg-int integer?)
  (define-symbolic* arg-int2 integer?)
  (stream-insn op streamidx arg2 arg3 option-index arg-int arg-int2))

(define (get-holes-list count)
  (for/list ([i (range count)]) (get-insn-holes)))

(define (get-retval-idx)
  (define-symbolic* retval-idx integer?)
  retval-idx)

;;;; other helpers

(define  (same program1 program2 . inputs)
  (equal? (apply program1 inputs)
                  (apply program2 inputs)))