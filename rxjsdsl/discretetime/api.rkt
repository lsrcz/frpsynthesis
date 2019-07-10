#lang racket

(require json)
(require "model.rkt")

(provide (all-defined-out))

;; insn-count : how many instructions/operators in sketch
;; input-count : how many arguments to synthesized function
;; bound : length of inputs to show how candidate functions differ
;; trace-list : a list of traces (see struct below)
(struct synth-query (insn-count input-count bound trace-list) #:transparent)

;; inputs : a list of inputs, in diagram format
;; output : a single output, in diagram format
;; diagrams are strings in which empty events are hyphens, e.g. "---3-2-12--3-"
(struct traces (inputs output) #:transparent)

;; cand-program-count : can be 0 (specs are unsatisfiable), 1 (only one program that matches spec in space), 2 (found 2 differing programs that match spec)
;; program1 : string representing candidate program 1 (can be empty string)
;; program2 : string representing candidate program 2 (can be empty string)
;; inputs : set of inputs on which program1 and program2 have different outputs
(struct query-result (cand-program-count program1 program2 inputs) #:transparent)

(define unsat-query-result (query-result 0 "" "" '()))

(define (load-query-from-file filename)
  (call-with-input-file filename
    (λ (in) (jsexpr->synth-query (string->jsexpr (port->string in))))))

(define (write-query-to-file filename query)
  (call-with-output-file filename
    (λ (out) (write-string (jsexpr->string (synth-query->jsexpr query)) out))
    #:exists 'replace))

(define (write-result-to-file filename result)
  (call-with-output-file filename
    (λ (out) (write-string (jsexpr->string (query-result->jsexpr result)) out))
    #:exists 'replace))

(define (query-result->jsexpr result)
  (make-hash (list (cons 'candidates (query-result-cand-program-count result))
                   (cons 'program1 (query-result-program1 result))
                   (cons 'program2 (query-result-program2 result))
                   (cons 'inputs (map stream->diagram (query-result-inputs result))))))

(define (jsexpr->synth-query json)
  (synth-query (hash-ref json 'insn-count)
               (hash-ref json 'input-count)
               (hash-ref json 'bound)
               (jsexpr->trace-list (hash-ref json 'trace-list))))

(define (jsexpr->trace-list json)
  (for/list ([trace json])
    (traces (map diagram->stream (hash-ref trace 'inputs))
            (diagram->stream (hash-ref trace 'output)))))

(define (jsexpr->stream json)
  (for/list ([evt json])
    (if (equal? evt "NOEVENT") NOEVENT evt)))

(define (synth-query->jsexpr query)
  (make-hash (list (cons 'insn-count (synth-query-insn-count query))
                   (cons 'input-count (synth-query-input-count query))
                   (cons 'bound (synth-query-bound query))
                   (cons 'trace-list (trace-list->jsexpr (synth-query-trace-list query))))))

(define (trace-list->jsexpr query-traces)
  (for/list ([trace query-traces])
    (make-hash (list (cons 'inputs (map stream->diagram (traces-inputs trace)))
                     (cons 'output (stream->diagram (traces-output trace)))))))

(define (stream->jsexpr stream)
  (for/list ([evt stream])
    (if (empty-event? evt) "NOEVENT" evt)))

;; assumption is that all events are integers
;; support for other event value types tk
;; note also that diagrams are jsexprs since they are strings
(define (diagram->stream diag)
  (map (λ (str) (if (equal? str "-") NOEVENT (string->number str)))
       (filter non-empty-string? (string-split diag ""))))

(define (stream->diagram stream)
  (string-join (for/list ([evt stream])
                 (if (empty-event? evt) "-" (number->string evt)))
                 ""))
