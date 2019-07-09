#! /usr/bin/env racket

#lang racket/base

(require "api.rkt")
(require "query.rkt")

(define args (current-command-line-arguments))
(define input-filename (vector-ref args 0))
(define output-filename (vector-ref args 1))

(define input-query (load-query-from-file input-filename))

(define result (make-synthesis-query input-query))

(write-result-to-file output-filename result)

(println "Success!")

