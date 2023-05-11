#lang racket
(provide interp-prim1 interp-prim2)
(require "heap.rkt")

;; Op1 Value* Heap -> Answer*
(define (interp-prim1 p v h)
  (match (list p v)
    [(list 'add1 (? integer? i))          (cons h (add1 i))]
    [(list 'sub1 (? integer? i))          (cons h (sub1 i))]
    [(list 'zero? (? integer? i))         (cons h (zero? i))]
    [(list 'char? v)                      (cons h (char? v))]
    [(list 'char->integer (? char?))      (cons h (char->integer v))]
    [(list 'integer->char (? codepoint?)) (cons h (integer->char v))]
    [(list 'eof-object? v)                (cons h (eof-object? v))]
    [(list 'write-byte (? byte?))         (cons h (write-byte v))]
    [(list 'box v)                        (alloc-box v h)]
    [(list 'unbox (list 'box i))          (cons h (heap-ref h i))]
    [(list 'car   (list 'cons i))         (cons h (heap-ref h i))]
    [(list 'cdr   (list 'cons i))         (cons h (heap-ref h (add1 i)))]
    [(list 'empty? v)                     (cons h (empty? v))]
    [(list 'integer? v)                   (cons h (integer? v))]
    [(list 'boolean? v)                   (cons h (boolean? v))]
    [_                                    'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim2 p v1 v2 h)
  (match (list p v1 v2)
    [(list '+ (? integer? i1) (? integer? i2)) (cons h (+ i1 i2))]
    [(list '- (? integer? i1) (? integer? i2)) (cons h (- i1 i2))]
    [(list '< (? integer? i1) (? integer? i2)) (cons h (< i1 i2))]
    [(list '= (? integer? i1) (? integer? i2)) (cons h (= i1 i2))]    
    [(list 'eq? v1 v2)
     (match (list v1 v2)
       [(list (list t1 a1)  (list t2 a2)) (cons h (and (eq? t1 t2) (= a1 a2)))]
       [_                                 (cons h (eqv? v1 v2))])]
    [(list 'cons v1 v2)                        (alloc-cons v1 v2 h)]
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))