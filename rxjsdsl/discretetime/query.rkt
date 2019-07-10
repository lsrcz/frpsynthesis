#lang rosette

(require "model.rkt")
(require "sketch.rkt")
(require "api.rkt")

(provide make-synthesis-query)

(define (make-symbolic-inputs query)
  (for/list ([i (range (synth-query-input-count query))])
    (new-event-stream get-sym-int (synth-query-bound query))))

(define (make-synthesis-query query)
  (let* ([sk (make-symbolic-sketch (synth-query-insn-count query) (synth-query-input-count query))]
         [sk-function (get-sketch-function sk)]
         [evaled-inputs (for/list ([inputs (map traces-inputs (synth-query-trace-list query))])
                          (apply sk-function inputs))]
         [m (synthesize #:forall '()
                        #:guarantee (for ([i (range (length evaled-inputs))])
                                      (assert (equal? (list-ref evaled-inputs i)
                                                      (list-ref (map traces-output (synth-query-trace-list query)) i)))))])
    (if (unsat? m)
        unsat-query-result
        (let* ([sk2 (make-symbolic-sketch (synth-query-insn-count query) (synth-query-input-count query))]
               [sk2-function (get-sketch-function sk2)]
               [evaled-inputs2 (for/list ([inputs (map traces-inputs (synth-query-trace-list query))])
                          (apply sk2-function inputs))]
               [sym-inputs (make-symbolic-inputs query)]
               [evaled-sym-inputs1 (apply (get-sketch-function (evaluate sk m)) sym-inputs)]
               [evaled-sym-inputs2 (apply (get-sketch-function sk2) sym-inputs)]
               [m2 (synthesize #:forall '()
                               #:guarantee (begin
                                             (assert (not (equal? evaled-sym-inputs1 evaled-sym-inputs2)))
                                             (for ([i (range (length evaled-inputs))])
                                               (assert (equal? (list-ref evaled-inputs2 i)
                                                               (list-ref (map traces-output (synth-query-trace-list query)) i))))))])
          (if (unsat? m2)
              (query-result 1 (string-from-sketch (evaluate sk m) "candidate-func") "" '())
              (query-result 2 (string-from-sketch (evaluate sk m) "candidate-func1")
                            (string-from-sketch (evaluate sk2 m2) "candidate-func2") (evaluate sym-inputs m2)))))))
