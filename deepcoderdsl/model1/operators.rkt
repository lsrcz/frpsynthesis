#lang rosette

(provide (all-defined-out))

(require "model.rkt")

(struct operator
  (name call print) #:transparent)

(define (call-stream-insn op insn past-vars)
  ((operator-call op) insn past-vars))

(define (print-stream-insn op insn varname past-vars)
    (format "  (define ~a (~a ~a))" varname (operator-name op) ((operator-print op) insn past-vars)))

;;;;;;;;;;; Deepcoder DSL ;;;;;;;;;

;; all functions have -dc suffix to distinguish them from Racket functions with the same names

(define NULL 'null)

;; head

(define (head-dc xs)
  (if (empty? xs)
      NULL
      (first xs)))

(define head-dc-op
  (operator "head-dc"
            (λ (insn past-vars) (head-dc (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; last

(define (last-dc xs)
  (if (empty? xs)
      NULL
      (last xs)))

(define last-dc-op
  (operator "head-dc"
            (λ (insn past-vars) (last-dc (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; take

(define (take-dc n xs)
  (if (negative? n)
      '()
      (if (> n (length xs))
          xs
          (take xs n))))

(define take-dc-op
  (operator "take-dc"
            (λ (insn past-vars) (take-dc (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))

;; drop

(define (drop-dc n xs)
  (if (negative? n)
      xs
      (if (>= n (length xs))
          '()
          (list-tail xs n))))

(define drop-dc-op
  (operator "drop-dc"
            (λ (insn past-vars) (drop-dc (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))

;; access

(define (access-dc n xs)
  (if (|| (negative? n) (>= n (length xs)))
      NULL
      (list-ref xs n)))


(define access-dc-op
  (operator "access-dc"
            (λ (insn past-vars) (access-dc (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))

;; minimum

(define (minimum-dc xs)
  (if (empty? xs)
      NULL
      (apply min xs)))

(define minimum-dc-op
  (operator "minimum-dc"
            (λ (insn past-vars) (minimum-dc (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; maximum

(define (maximum-dc xs)
  (if (empty? xs)
      NULL
      (apply max xs)))

(define maximum-dc-op
  (operator "maximum-dc"
            (λ (insn past-vars) (maximum-dc (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; reverse

(define (reverse-dc xs)
  (reverse xs))

(define reverse-dc-op
  (operator "reverse-dc"
            (λ (insn past-vars) (reverse-dc (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; sort

(define (sort-dc xs)
  (sort xs <))

(define sort-dc-op
  (operator "sort-dc"
            (λ (insn past-vars) (sort-dc (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; sum

(define (sum-dc xs)
  (list (foldl + 0 xs)))

(define sum-dc-op
  (operator "sum-dc"
            (λ (insn past-vars) (sum-dc (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

;; map

(define (map-dc f xs)
  (map f xs))

(define map-dc-op
  (operator "map-dc"
            (λ (insn past-vars) (map-dc (list-ref int-to-int-funcs (get-option-index insn)) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref int-to-int-funcs-string (get-option-index insn)) (get-input-stream insn past-vars)))))

;; filter

#;(define (filter-dc f xs)
  (filter f xs))
(define (filter-dc pred xs)
  (letrec ([f (λ (xs)
                (cond [(not (list? xs)) '()]
                      [(empty? xs) '()]
                      [(pred (first xs)) (append (list (first xs)) (f (cdr xs)))]
                      [else (f (cdr xs))]))])
        (f xs)))

(define filter-dc-op
  (operator "filter-dc"
            (λ (insn past-vars) (filter-dc (list-ref int-to-bool-funcs (get-option-index insn)) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref int-to-bool-funcs (get-option-index insn)) (get-input-stream insn past-vars)))))

;; count

(define (count-dc f xs)
  (count f xs))

(define count-dc-op
  (operator "count-dc"
            (λ (insn past-vars) (count-dc (list-ref int-to-bool-funcs (get-option-index insn)) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref int-to-bool-funcs (get-option-index insn)) (get-input-stream insn past-vars)))))

;; zipWith

(define (zipwith-dc f xs1 xs2)
  (let ([xs-min (min (length xs1) (length xs2))])
    (map f (take xs1 xs-min) (take xs2 xs-min))))

(define zipwith-dc-op
  (operator "zipwith-dc"
            (λ (insn past-vars) (zipwith-dc (list-ref int-to-int-to-int-funcs (get-option-index insn)) (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref int-to-int-to-int-funcs-string (get-option-index insn)) (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))))

;; scanl1

(define (scanl1-dc f xs)
  (if (empty? xs)
      '()
      (for/list ([i (range (length xs))])
        (foldl f (first xs) (take (drop xs 1) i)))))

(define scanl1-dc-op
  (operator "scanl1-dc"
            (λ (insn past-vars) (scanl1-dc (list-ref int-to-int-to-int-funcs (get-option-index insn)) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref int-to-int-to-int-funcs-string (get-option-index insn)) (get-input-stream insn past-vars)))))


(define int-to-int-funcs (list (λ (i) (+ i 1))
                               (λ (i) (- i 1))
                               (λ (i) (+ i i))
                               (λ (i) (quotient i 2))
                               (λ (i) (* i -1))
                               (λ (i) (expt i 2))
                               (λ (i) (+ i i i))
                               (λ (i) (quotient i 3))
                               (λ (i) (+ i i i i))
                               (λ (i) (quotient i 4))
                               ))
(define int-to-int-funcs-string (list "(λ (i) (+ i 1))"
                                      "(λ (i) (- i 1))"
                                      "(λ (i) (+ i i))"
                                      "(λ (i) (quotient i 2))"
                                      "(λ (i) (* i -1))"
                                      "(λ (i) (expt i 2))"
                                      "(λ (i) (* i 3))"
                                      "(λ (i) (quotient i 3))"
                                      "(λ (i) (* i 4))"
                                      "(λ (i) (quotient i 4))"
                                      ))

(define int-to-bool-funcs (list (λ (i) (< i 0))
                                (λ (i) (> i 0))
                                (λ (i) (equal? 0 (modulo i 2)))
                                (λ (i) (equal? 1 (modulo i 2)))
                                ))
(define int-to-bool-funcs-string (list "(λ (i) (negative? i))"
                                       "(λ (i) (positive? i))"
                                       "(λ (i) (even? i))"
                                       "(λ (i) (odd? i))"
                                       ))

(define int-to-int-to-int-funcs (list +
                                      -
                                      *
                                      min
                                      max
                                      ))
(define int-to-int-to-int-funcs-string (list "+"
                                             "-"
                                             "*"
                                             "min"
                                             "max"))

(define operator-list (list head-dc-op
                            last-dc-op
                            take-dc-op
                            drop-dc-op
                            access-dc-op
                            minimum-dc-op
                            maximum-dc-op
                            reverse-dc-op
                            sort-dc-op
                            sum-dc-op
                            map-dc-op
                            filter-dc-op
                            count-dc-op
                            zipwith-dc-op
                            scanl1-dc-op))

(define (get-operator op-code)
  (list-ref operator-list op-code))
