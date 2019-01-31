#lang rosette

(require "../model1/model.rkt")
(require "../model1/operators.rkt")
(require "../model1/sketch.rkt")

(define stream-length 4)

(define sym-time (new-behavior get-sym-int stream-length))
(define sym-temp (new-behavior get-sym-int stream-length))

(define (thermostat-func time temp)
  (ifB (andB (liftB (λ (e) (< e 55)) temp)
             (andB (liftB (λ (e) (< e 18)) time)
                   (liftB (λ (e) (> e 8)) time)))
       (constantB 'on temp) (constantB 'off temp)))

(define (simpler-func time temp)
  (ifB (liftB (λ (e) (< e 55)) temp)
       (constantB 'on temp)
       (constantB 'off temp)))

(define (simplest-func time temp)
  (liftB (λ (e) (< e 55)) temp))

(define sk (get-symbolic-sketch 8 2))

(define evaled-sk ((get-sketch-function sk) sym-time sym-temp))

(define b (time (synthesize #:forall (symbolics (list sym-time sym-temp))
                            #:guarantee (assert (equal? evaled-sk (thermostat-func sym-time sym-temp))))))