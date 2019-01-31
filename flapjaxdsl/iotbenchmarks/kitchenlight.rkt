#lang rosette

(require "../model1/model.rkt")
(require "../model1/operators.rkt")
(require "../model1/sketch.rkt")

(define stream-length 4)

(define NIGHT 0)
(define HOME 1)
(define AWAY 2)

(define LIGHT-OFF 0)
(define LIGHT-WHITE 1)
(define LIGHT-ORANGE 2)

(define sym-time (new-behavior get-sym-int stream-length))
(define sym-location (new-behavior get-sym-bool stream-length))
(define sym-motionSensor (new-behavior get-sym-bool stream-length))

(define (mode-func time location)
  (ifB (andB (liftB (λ (e) (> e 19)) time)
             (liftB (λ (e) (< e 8)) time))
       (constantB NIGHT location)
       (ifB location
            (constantB HOME location)
            (constantB AWAY location))))

(define (light-func time location motionSensor)
  (ifB motionSensor
       (ifB (liftB (λ (e) (equal? NIGHT e)) (mode-func time location))
            (constantB LIGHT-ORANGE location)
            (constantB LIGHT-WHITE location))
       (constantB LIGHT-OFF location)))

(define sk (get-symbolic-sketch 14 3))

(define evaled-sk ((get-sketch-function sk) sym-time sym-location sym-motionSensor))

(define b (time (synthesize #:forall (symbolics (list sym-time sym-location))
                            #:guarantee (assert (equal? evaled-sk (light-func sym-time sym-location sym-motionSensor))))))