#lang rosette

(require "../model1/model.rkt")
(require "../model1/operators.rkt")
(require "../model1/sketch.rkt")

(define stream-length 4)

(define (sym-coords)
  (define-symbolic* x integer?)
  (define-symbolic* y integer?)
  (cons x y))

(define sym-mouse-up (new-event-stream get-sym-bool stream-length))
(define sym-mouse-down (new-event-stream get-sym-bool stream-length))
;(define sym-mouse-pos (new-event-stream sym-coords stream-length))
(define sym-mouse-pos (new-event-stream get-sym-int stream-length))

(define (draganddrop-func mouse-up mouse-down mouse-pos)
  (ifE (snapshotE mouse-pos (startsWith #f (mergeE (constantE #t mouse-down)
                                                   (constantE #f mouse-up))))
       mouse-pos
       (zeroE mouse-pos)))

#;(define (draganddrop-func mouse-up mouse-down mouse-pos)
  (snapshotE mouse-pos (startsWith #f (mergeE (constantE #t mouse-down)
                                              (constantE #f mouse-up)))))
#;(define (draganddrop-func mouse-up mouse-down mouse-pos)
  (ifE mouse-up
       mouse-pos
       (zeroE mouse-pos)))

(define (zero-func m)
  (zeroE m))

(define (small-func u d p)
  (snapshotE p (startsWith #f (mergeE (constantE #t d) (constantE #f u)))))

(define sk (get-symbolic-sketch 8 3))
(define evaled-sk ((get-sketch-function sk) sym-mouse-up sym-mouse-down sym-mouse-pos))

(define b (time (synthesize #:forall (symbolics (list sym-mouse-up sym-mouse-down sym-mouse-pos))
                            #:guarantee (assert (equal? evaled-sk (draganddrop-func sym-mouse-up sym-mouse-down sym-mouse-pos))))))

(if (unsat? b)
    (println "unsat")
    (print-sketch sk b))

;; synthesized with 8 insn sketch
;;cpu time: 552388 real time: 5750701 gc time: 6080
;;(define (synthesized-function input1 input2 input3)
;;  (define r1 input1)
;;  (define r2 input2)
;;  (define r3 input3)
;;  (define r4 (constantE arg-int$0 r2))
;;  (define r5 (constantE #f r1))
;;  (define r6 (mergeE r4 r5))
;;  (define r7 (startsWith #f r6))
;;  (define r8 (snapshotE r3 r7))
;;  (define r9 (andE r3 r8))
;;  (define r10 (snapshotE r9 r7))
;;  (define r11 (ifE r10 r3 r10))
;;  r11)