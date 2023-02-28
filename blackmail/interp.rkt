#lang racket
(provide interp)
(require "ast.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Int i) i]
    [(Prim1 op e) (interp-prim op (interp e))]
    ))

(define (interp-prim op i)
  (match op
    ['add1 (+ i 1)]
    ['sub1 (- i 1)]))
