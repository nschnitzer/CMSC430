#lang racket
(provide (all-defined-out))
(require "ast.rkt" a86/ast)
 
;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))
 
;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Prim1 p e) (compile-prim1 p e)]
    [(Int i)     (compile-integer i)]
    [(IfZero e1 e2 e3) (compile-ifzero e1 e2 e3)]))
 
;; Op Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax 1)]
         ['sub1 (Sub 'rax 1)])))
 
;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax i)))

(define (compile-prim p e)
  (match p
    ['add1 (seq (compile-e e) (Add 'rax 1))]
    ['sub1 (seq (compile-e e) (Sub 'rax 1))]))

;; Expr Expr Expr -> Asm
(define (compile-ifzero e1 e2 e3)
  (let ((done (gensym "if"))
        (els (gensym "els")))
  (seq 
       (compile-e e1) ;; Puts e1 in 'rax
       (Cmp 'rax 0) ;; Compares 'rax to 0
       (Jne els)
       (compile-e e2)
       (Jmp done)
       (Label els)
       (compile-e e3)
       (Label done))))
       

