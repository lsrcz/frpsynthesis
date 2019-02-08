#lang rosette

(require "../model1/model.rkt")
(require "../model1/operators.rkt")
(require "../model1/sketch.rkt")

(define stream-length 4)

(define one-text-inputs (list NOEVENT NOEVENT NOEVENT))
(define one-save-btn-clicks (list NOEVENT 'click NOEVENT))
(define one-cron-job-5min (list NOEVENT NOEVENT NOEVENT))
(define one-save-draft (list NOEVENT NOEVENT NOEVENT))

(define two-text-inputs (list 'change NOEVENT NOEVENT))
(define two-save-btn-clicks (list NOEVENT 'click NOEVENT))
(define two-cron-job-5min (list NOEVENT NOEVENT NOEVENT))
(define two-save-draft (list NOEVENT #t NOEVENT))

(define text-inputs (new-event-stream (λ () 'change) stream-length))
(define save-btn-clicks (new-event-stream (λ () 'click) stream-length))
(define cron-job-5min (new-event-stream (λ () 'five) stream-length))

(define (save-draft-func text-inputs save-btn-clicks cron-job-5min)
  (let ([filtered (mergeE (constantE #f text-inputs) (constantE #t (mergeE save-btn-clicks cron-job-5min)))])
    (andE (snapshotE filtered (startsWith #f (constantE #t text-inputs)))
          (filterRepeatsE filtered))))

(define sk (get-symbolic-sketch 9 3))
(define evaled-sk ((get-sketch-function sk) text-inputs save-btn-clicks cron-job-5min))

(define b (time (synthesize #:forall (symbolics (list text-inputs save-btn-clicks cron-job-5min))
                            #:guarantee (assert (equal? evaled-sk (save-draft-func text-inputs save-btn-clicks cron-job-5min))))))

(if (unsat? b)
    (println "unsat")
    (print-sketch sk b))