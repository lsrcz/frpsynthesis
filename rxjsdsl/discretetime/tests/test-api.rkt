#lang racket

(require rackunit)

(require "../model.rkt")
(require "../api.rkt")

(define input1 (list NOEVENT 1 3 NOEVENT 7))
(define output (list NOEVENT 2 4 NOEVENT 8))
(define skq (synth-query 1 1 4 (list (traces (list input1) output))))
(define skq2 (synth-query 1 1 4
                          (list (traces (list (list NOEVENT 0 NOEVENT 0))
                                        (list NOEVENT 1 NOEVENT 1)))))

(check-equal? (stream->diagram input1) "-13-7")
(check-equal? (diagram->stream "-13-7") input1)