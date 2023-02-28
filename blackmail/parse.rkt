#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? exact-integer?) (Int s)]
    [(list (? op? o) e) (Prim1 o (parse e))]
    [_ (error "Parse Error")]))

;; Any -> Boolean
(define (op? x)
  (memq x '(add1 sub1)))
