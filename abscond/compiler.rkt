#lang racket

(provide compile)

(require "ast.rkt" a86/ast)

(define (compile e)
  (prog (Global 'nice)
        (Label 'nice)
        (compile-e e)
        (Ret)))

; AST -> a86
(define (compile-e e)
  (match e
    [(Int i) (seq (Mov 'rax i))]))


