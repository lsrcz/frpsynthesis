#lang rosette

(require "model.rkt")

(provide (all-defined-out))

(define NOTDECIDED 'not-decided)

(struct trace (inputs output) #:transparent)

(define (equal-decided? o1 o2)
  (define (equal-one-decided o1 o2)
    (or (eq? o1 NOTDECIDED) (eq? o2 NOTDECIDED) (eq? o1 o2)))
  (if (null? o1)
      #t
      (and (equal-one-decided (car o1) (car o2)) (equal-decided? (cdr o1) (cdr o2)))))

(define (equal-on-trace? f t)
  (let ([output1 (apply f (trace-inputs t))]
        [output2 (trace-output t)])
    (equal-decided? output1 output2)))

(define (gen-trace . inputs)
  (trace inputs (for/list ([i (car inputs)]) NOTDECIDED)))

(define (mutate-trace-x t pos outval)
  (define (new-output pos outlist outval)
    (if (eq? pos 0)
        (cons outval (cdr outlist))
        (cons (car outlist) (new-output (- pos 1) (cdr outlist) outval))))
  (trace (trace-inputs t) (new-output pos (trace-output t) outval)))

(define (clear-trace-x t pos)
  (define (new-output pos outlist)
    (if (eq? pos 0)
        (cons NOTDECIDED (cdr outlist))
        (cons (car outlist) (new-output (- pos 1) (cdr outlist)))))
  (trace (trace-inputs t) (new-output pos (trace-output t))))

(define (accept-trace-all t concrete)
  (trace (trace-inputs t) concrete))

(define (apply-trace f trace)
  (apply f (trace-inputs trace)))

(define (partial-trace? t)
  (ormap (λ (x) (eq? x NOTDECIDED)) (trace-output t)))

(define (full-trace? t)
  (not (partial-trace? t)))

(define (gen-full-spec-constraint f t)
  (equal? (apply-trace f t) (trace-output t)))

(define (gen-par-spec-constraint f t)
  (equal-decided? (apply-trace f t) (trace-output t)))

(define (print-func event)
  (if (null? (symbolics event))
      (cond [(eq? NOTDECIDED event) "UND"]
            [(eq? NOEVENT event) "NOEVT"]
            [else (format "~a" event)])
      "SYM"))
(define (print-lst-with-len lst len-lst)
  (define (inner-iter lst len-lst)
    (if (null? lst)
        (void)
        (begin
          (display (car lst))
          (display (build-string (- (car len-lst) (string-length (car lst))) (λ (i) #\space)))
          (inner-iter (cdr lst) (cdr len-lst)))))
  (display "(")
  (inner-iter lst len-lst)
  (display ")")
  (newline))

(define (print-trace tr)
  (let* ([input-strs
          (map (λ (lst)
                 (map print-func lst))
               (trace-inputs tr))]
         [output-str (map print-func (trace-output tr))]
         [str-length (map (λ (lst) (map string-length lst)) (cons output-str input-strs))]
         [out-length (map add1 (apply map max str-length))])
    (displayln "Trace inputs:")
    (for/list ([input input-strs])
      (print-lst-with-len input out-length))
    (displayln "Trace output:")
    (print-lst-with-len output-str out-length)))

(define (print-trace-with-concrete tr concrete-output)
  (let* ([input-strs
          (map (λ (lst)
                 (map print-func lst))
               (trace-inputs tr))]
         [output-str (map print-func (trace-output tr))]
         [concrete-str (map print-func concrete-output)]
         [str-length (map (λ (lst) (map string-length lst)) (list* output-str concrete-str input-strs))]
         [out-length (map add1 (apply map max str-length))])
    (displayln "Trace inputs:")
    (for/list ([input input-strs])
      (print-lst-with-len input out-length))
    (displayln "Trace output:")
    (print-lst-with-len output-str out-length)
    (displayln "Concrete output:")
    (print-lst-with-len concrete-str out-length)))

