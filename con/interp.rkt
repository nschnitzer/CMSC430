#lang racket
(provide interp)
(require "ast.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Int i) i]
    [(Prim1 op e) (interp-prim op (interp e))]
    [(IfZero e1 e2 e3)
     (if (zero? (interp e1))
         (interp e2)
         (interp e3))]))

(define (interp-prim op i)
  (match op
    ['add1 (+ i 1)]
    ['sub1 (- i 1)]))

