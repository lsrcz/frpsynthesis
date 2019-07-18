#lang rosette

(require "../discretetime/model.rkt")
(require "../discretetime/dsl.rkt")
(require "../discretetime/sketch.rkt")
(require "../discretetime/operators.rkt")
(require "../discretetime/constraints.rkt")
(require rosette/lib/match)

(struct ball (x y) #:transparent)
(define (get-sym-ball)
  (define-symbolic* x integer?)
  (define-symbolic* y integer?)
  (ball x y))


(define ball-stream (list
                     NOEVENT
                     (ball 50 295)
                     (ball 30 290)
                     (ball 30 295)
                     (ball 30 300)
                     (ball 31 290)
                     (ball 31 295)
                     (ball 31 300)
                     (ball 80 290)
                     (ball 80 295)
                     (ball 80 300)
                     (ball 129 300)
                     (ball 130 300)
                     NOEVENT
                     (ball 130 300)
                     NOEVENT
                     (ball 130 300)
                     NOEVENT
                     (ball 130 300)
                     ))
(define paddle-stream (list
                       80
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       NOEVENT
                       81
                       NOEVENT
                       179
                       NOEVENT
                       180
                       NOEVENT
                       ))
(define cpaddle-stream (list
                        NOEVENT
                        #t
                        NOEVENT
                        NOEVENT
                        NOEVENT
                        NOEVENT
                        NOEVENT
                        #t
                        NOEVENT
                        NOEVENT
                        #t
                        #t
                        NOEVENT
                        NOEVENT
                        #t
                        NOEVENT
                        #t
                        NOEVENT
                        NOEVENT))

(define CANVAS_HEIGHT 320)
(define PADDLE_HEIGHT 20)
(define BALL_RADIUS 10)
(define PADDLE_WIDTH 100)

(define (filter1 b)
  (match b
    [(ball x y)
     (> y
        (- CANVAS_HEIGHT
           (+ PADDLE_HEIGHT
              (quotient BALL_RADIUS 2))))]
    ))
(define (filter2 input)
  (match input
    [(list (ball x y) p)
     (> x
        (- p (quotient PADDLE_WIDTH 2)))]))

(define (filter3 input)
  (match input
    [(list (ball x y) p)
     (< x
        (+ p (quotient PADDLE_WIDTH 2)))]))

(define (target-func i1 i2)
  (define r1 i1)
  (define r2 i2)
  (define r3 (rxFilter filter1 r1))
  (define r4 (rxWithLatestFrom r3 r2))
  (define r5 (rxFilter filter2 r4))
  (define r6 (rxFilter filter3 r5))
  (define r7 (rxMap (lambda (x) #t) r6))
  r7)

(set-unary-functions (list filter1 filter2 filter3 (lambda (i) #t))  (list "filter1" "filter2" "filter3" "(lambda (i) #t)")
                     (list "filter1" "filter2" "filter3" "(lambda (i) #t)"))


(define sym-sk (make-symbolic-sketch 5 2))

;(define evaled-sk ((get-sketch-function sym-sk) ball-stream paddle-stream))

(define evaled-sk ((get-sketch-function sym-sk) ball-stream paddle-stream))

(define stream-length 8)
(define sym-ball-forall (new-event-stream get-sym-ball stream-length))
(define sym-paddle-forall (new-event-stream get-sym-int stream-length))
(define sk-constraint ((get-sketch-constraint sym-sk) sym-ball-forall sym-paddle-forall))

(define m (time (synthesize #:forall (symbolics (list sym-ball-forall sym-paddle-forall))
                            #:guarantee (begin
                                          (assert (equal? evaled-sk  (target-func ball-stream paddle-stream)))
                                          (assert sk-constraint)))))

(print-sketch sym-sk m)


(define sym-ball (new-event-stream get-sym-ball stream-length))
(define sym-paddle (new-event-stream get-sym-int stream-length))
(define (ballXConstraint b)
  (cond [(null? b) #t]
        [(empty-event? (car b)) (ballXConstraint (cdr b))]
        [((lambda (a)
            (match a
              [(ball x y)
               (and (> x 10)
                    (< x 470))])) (car b))
         (ballXConstraint (cdr b))]
        [else #f]))

(define (paddleConstraint p)
  (cond [(null? p) #t]
        [(empty-event? (car p)) (paddleConstraint (cdr p))]
        [((lambda (a) (and (>= a 50) (<= a 430))) (car p)) (paddleConstraint (cdr p))]
        [else #f]))

(define concrete-sk (evaluate sym-sk m))
(define evaled-sk1 ((get-sketch-function concrete-sk) sym-ball sym-paddle))
(define evaled-sk1-ori ((get-sketch-function concrete-sk) ball-stream paddle-stream))
(define sym-sk2 (make-symbolic-sketch 6 2))
(define evaled-sk2 ((get-sketch-function sym-sk2) sym-ball sym-paddle))
(define evaled-sk2-ori ((get-sketch-function sym-sk2) ball-stream paddle-stream))
(define sk2-constraint ((get-sketch-constraint sym-sk2) sym-ball-forall sym-paddle-forall))

(define b2 (time (synthesize #:forall (symbolics (list sym-ball-forall sym-paddle-forall))
                             #:guarantee (begin
                                           (assert (exactly-one-event sym-ball sym-paddle))
                                           (assert (ballXConstraint sym-ball))
                                           (assert (paddleConstraint sym-paddle))
                                           (assert (equal? evaled-sk2-ori evaled-sk1-ori))
                                           (assert sk2-constraint)
                                           (assert (not (equal? evaled-sk1 evaled-sk2)))))))

(print-sketch sym-sk2 b2)
(println (evaluate evaled-sk2-ori b2))
(println (evaluate sym-ball b2))
(println (evaluate sym-paddle b2))

(println (evaluate evaled-sk1 b2))
(println (evaluate evaled-sk2 b2))
