#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? exact-integer?) (Int s)]
    [(list (? op? o) e) (Prim1 o (parse e))]
    [(list 'if (list 'zero? e1) e2 e3)
       (IfZero (parse e1) (parse e2) (parse e3))]
    [_ (error "Parse Error")]))

;; Any -> Boolean
(define (op? x)
  (memq x '(add1 sub1)))
