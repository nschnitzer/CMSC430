#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

(define rax 'rax)
(define rbx 'rbx)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)       (compile-value i)]
    [(Bool b)      (compile-value b)]
    [(Prim1 p e)   (compile-prim1 p e)]
    [(If e1 e2 e3) (compile-if e1 e2 e3)]
    [(Cond cls el) (compile-cond cls el)]
    [(Case target clauses els) (compile-case target clauses els)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

(define (compile-cond-cls-list cls end)
  (match cls
    [(cons cl clss) (seq  (compile-cl cl end)
                          (compile-cond-cls-list clss end))]
    ['() (seq)]))

(define (compile-cl c end)
  (match c
    [(Clause p b)   (let ((l (gensym 'cl)))
                        (seq  (compile-e p)
                              (Cmp rax val-false)
                              (Je l)
                              (compile-e b)
                              (Jmp end)
                              (Label l)))]
    [_ (seq )]))


;; [Listof CondClause] Expr -> Asm
(define (compile-cond cls el)
    (let ((end (gensym 'cond)))
        (seq (compile-cond-cls-list cls end)
             (compile-e el)
             (Label end)))
    )

(define (compile-case target clauses els)
    (let ((case-end (gensym 'case)))
      (seq  (compile-e target) ; put target in rax
            (compile-case-cls-list clauses case-end)
            (compile-e els) ; 
            (Label case-end)))) ; should be jumped to if one of the cases are met after putting the clause expr in rax

(define (compile-case-cls-list cls case-end)
  (match cls
    [(cons cl clss)   (seq  ; target should be in rax
                        (compile-case-cl cl case-end)
                        (compile-case-cls-list clss case-end))] ; Make sure to put the processed target in rax
    ['()              (seq )]))

(define (compile-case-cl c case-end)
  (match c
    [(Clause datums b)  (let ((meets-clause (gensym 'meetsClause))
                              (fails-clause (gensym 'failsClause)))
                      (seq  
                          (compile-case-datum datums meets-clause fails-clause)
                          (Label meets-clause)
                          (compile-e b) ; puts val of case clause expr in rax
                          (Jmp case-end)
                          (Label fails-clause)
                          ; If here, the value of rax should be the target val
                          ))]
      [_                  (error "Not a clause")]))

; Basically running a cond on if any of the datum match the target expression
(define (compile-case-datum dtm meets-clause fails-clause)
  (match dtm
    [(cons d ds)   (let ((d1 (gensym 'endDatum)))
                      (seq  (Push rax) ; rax should contain the value of the expression to be matched, so store it in the stack
                            (compile-e d) ; now compiled d is in rax
                            (Pop rbx) ; bring back the target expr
                            (Cmp rax rbx)
                            (Mov rax rbx) 
                            (Je meets-clause) ; Jumps to meets-clause if a match
                            (Label d1) ; Here to make debugging earlier
                            (compile-case-datum ds meets-clause fails-clause)))] ; Moves onto the other datum to check
    ['() (seq (Jmp fails-clause))]))