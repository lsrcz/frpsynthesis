#lang rosette
(provide (all-defined-out))
(require "model.rkt")

(define (trace x)
  (begin
    (println x)
    x))

(define (exactly-one-event . streams)
  (null? (filter (lambda (x) (not x))
                 (apply map (lambda allvalue
                              (let ([newlst (filter
                                             (lambda (x) (not (empty-event? x)))
                                             allvalue)])
                                (and (not (null? newlst)) (null? (cdr newlst)))))
                        streams))))

(define (less-than-one-event . streams)
  (null? (filter (lambda (x) (not x))
                 (apply map (lambda allvalue
                              (let ([newlst (filter
                                             (lambda (x) (not (empty-event? x)))
                                             allvalue)])
                                (or (null? newlst) (null? (cdr newlst)))))
                        streams))))




