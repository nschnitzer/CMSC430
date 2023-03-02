#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax)
(define r9 'r9)   ; scratch

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (Add rax (value->bits 1))]
    ['sub1 (Sub rax (value->bits 1))]
    ['zero?
     (seq (Cmp rax 0)
          (Mov rax (value->bits #f))
          (Mov r9  (value->bits #t))
          (Cmove rax r9))]
    ['abs 
      (let ((neg (gensym 'abs))
            (pos (gensym 'abs)))
        (seq  (Cmp rax 0)
              (Jl neg)
              (Jmp pos)
              (Label neg)
              (flip-sign)
              (Label pos)))]
    ['- (flip-sign)]
    ['not (let ((ret_true (gensym 'not))
                (ret_val (gensym 'not)))
            (seq  (Mov r9 (value->bits #f))
                  (Cmp rax r9)
                  (Je ret_true)
                  (Mov rax (value->bits #f))
                  (Jmp ret_val)
                  (Label ret_true)
                  (Mov rax (value->bits #t))
                  (Label ret_val)))]))

(define (flip-sign)
  (seq (Mov r9 (value->bits 0))
       (Sub r9 rax)
       (Mov rax r9)))
