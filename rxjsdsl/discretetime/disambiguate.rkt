#lang rosette

(require "model.rkt")
(require "correctloop.rkt")
(require "sketch.rkt")

(provide (all-defined-out))

(define (update-trace trace concrete)
  (define (iter trace)
    (displayln "Please input command, h for help")
    (let ([command (read)])
      (cond [(eq? command 'h)
             (displayln "c pos    : clear")
             (displayln "p        : print")
             (displayln "s pos val: set")
             (displayln "a pos    : accept the output at pos")
             (displayln "aa       : accept all")
             (displayln "f        : finish providing new trace val")
             (displayln "ok       : use this model")
             (iter trace)]
            [(eq? command 'ok)
             'ok]
            [(eq? command 'c)
             (let ([pos (read)])
               (iter (clear-trace-x trace pos)))]
            [(eq? command 'p)
             (begin
               (displayln (map print-func (trace-output trace)))
               (iter trace))]
            [(eq? command 'e)
             trace]
            [(eq? command 'a)
             (let ([pos (read)])
               (iter (mutate-trace-x trace pos (list-ref concrete pos))))]
            [(eq? command 'aa)
             (iter (accept-trace-all trace concrete))]
            [(eq? command 's)
             (let ([pos (read)]
                   [val (read)])
               (iter (mutate-trace-x trace pos val)))]
            [else
             (displayln "Unknown command")
             (iter trace)])))
  (iter trace))

(struct gen-result (model trace) #:transparent)

(define (gen-new-par-trace sketch model full-traces len constructor-list)
  (displayln "------")
  (displayln full-traces)
  (displayln "!------!")
  (define concrete-func (get-sketch-function (evaluate sketch model)))
  (define sk-func (get-sketch-function sketch))
  (define inputs
    (for/list ([constructor constructor-list])
      (new-event-stream constructor len)))
  (define full-spec-constraint (andmap (λ (t) (gen-full-spec-constraint sk-func t)) full-traces))
  (define m
    (synthesize #:forall (list)
                #:guarantee (assert (and full-spec-constraint
                                         (not (equal? (apply concrete-func inputs)
                                                      (apply sk-func inputs)))))))
  
  (if (unsat? m)
      'unsat
      (begin

        (let ([new-trace (apply gen-trace (evaluate inputs (complete-solution m (symbolics (list inputs sketch)))))])
          (print-sketch sketch m)
          (println new-trace)
          (print-trace new-trace)
          (displayln (apply-trace concrete-func new-trace))
          (displayln (apply-trace (get-sketch-function (evaluate sketch m)) new-trace))
          (displayln "!!------!!")
          (gen-result m new-trace)))))
  
(define (synthesize-by-disambiguate sketch init-par-trace len constructor-list)
  (define sk-func (get-sketch-function sketch))
  (define (full-spec-synth full-traces model)
    (let ([new-trace (gen-new-par-trace sketch model full-traces len constructor-list)])
      (if (eq? 'unsat new-trace)
          model
          (partial-spec-dispatch full-traces (gen-result-trace new-trace) model))))
  (define (partial-spec-dispatch full-traces updated-trace model)
    (if (eq? updated-trace 'ok)
        model
        (if (partial-trace? updated-trace)
            (partial-spec-synth full-traces updated-trace)
            (full-spec-synth (cons updated-trace full-traces) model))))
  (define (partial-spec-update full-traces partial-trace model concrete)
    (let ([updated-trace (update-trace partial-trace concrete)])
      (partial-spec-dispatch full-traces updated-trace model)))
  (define (partial-spec-synth full-traces partial-trace)
    (define full-spec-constraint (andmap (λ (t) (gen-full-spec-constraint sk-func t)) full-traces))
    (define par-spec-constraint (gen-par-spec-constraint sk-func partial-trace))
    (define constraint (and full-spec-constraint par-spec-constraint))
    (define model (synthesize #:forall (list)
                              #:guarantee (assert constraint)))
    (if (unsat? model)
        (displayln "Unsat!")
        (begin
          (displayln "Synthesize with partial contraint: ")
          (print-sketch sketch model)
          (print-sketch-rx sketch model)
          (let ([concrete (apply-trace (get-sketch-function (evaluate sketch model)) partial-trace)])
            (print-trace-with-concrete partial-trace concrete)
                                     
            (partial-spec-update full-traces partial-trace model concrete)))))
  (partial-spec-synth '() init-par-trace))