#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define rbx 'rbx) ; spare

;; type CEnv = (Listof ID)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Global 'entry)
        (Label 'entry)
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        pad-stack
        (Call 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Var x)            (compile-variable x c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitives
    [(PrimN p es)       (compile-primN p es c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    [(Let x e1 e2)      (seq (compile-e* e1 c)
                             (compile-let x e2 c))]
    ;; TODO: implement let*, case, cond
    [(Let* xs es e)  (compile-let* xs es e c)]
    [(Case ev cs el) (compile-case ev cs el c)]
    [(Cond cs el)    (compile-cond cs el c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

;; Might be better to have all of the args sent to the stack to start
(define (compile-primN p es c)
  (match es
    ['() (seq (Mov rax 0))]
    ;;; [(cons x '()) (seq
    ;;;                 (compile-e x c))] ;; Put the last arg in rax
    [(cons x xs)  (seq
                    (compile-e x c) ;; Resolves this arg into rax
                    (Push rax) ;; Push onto stack
                    (compile-primN p xs (cons #f c)) ;; Will be in rax
                    (compile-op2 p) ;; this val on top of stack and the following are in rax
                    ;; Resulting value of the operation is in rax
                    )]
  )
)

;; HINT: Another potentially helpful function that
;; emits code to execute each expression and push
;; all the values on to the stack, analogous to interp*-env

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es c))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

(define (compile-let vs e2 c)
  (match vs
    [(cons x xs) (seq
                    (compile-let xs e2 (cons x c))
                    (Add rsp 8))]
    ['() (seq (compile-e e2 c))]
  )
)

(define (compile-let* vs e1 e2 c)
  (match e1
    ['()  (seq 
            (compile-e e2 c)
            )]
    [(cons x xs)  (seq
                    (compile-e x c)
                    (Push rax)
                    (compile-let* (cdr vs) xs e2 (cons (first vs) c))
                    (Add rsp 8)
                    )]
  )
)

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; [Listof CondClause] Expr -> Asm
(define (compile-cond cls el c)
    (let ((end (gensym 'cond)))
        (seq (compile-cond-cls-list cls end c)
             (compile-e el c)
             (Label end)))
    )


(define (compile-cl c end c1)
  (match c
    [(Clause p b)   (let ((l (gensym 'cl)))
                        (seq  (compile-e p c1)
                              (Cmp rax val-false)
                              (Je l)
                              (compile-e b c1)
                              (Jmp end)
                              (Label l)))]
    [_ (seq )]))

(define (compile-cond-cls-list cls end c)
  (match cls
    [(cons cl clss) (seq  (compile-cl cl end c)
                          (compile-cond-cls-list clss end c))]
    ['() (seq)]))

(define (compile-case target clauses els c)
    (let ((case-end (gensym 'case)))
      (seq  (compile-e target c) ; put target in rax
            (compile-case-cls-list clauses case-end c)
            (compile-e els c) ; 
            (Label case-end)))) ; should be jumped to if one of the cases are met after putting the clause expr in rax

(define (compile-case-cls-list cls case-end c)
  (match cls
    [(cons cl clss)   (seq  ; target should be in rax
                        (compile-case-cl cl case-end c)
                        (compile-case-cls-list clss case-end c))] ; Make sure to put the processed target in rax
    ['()              (seq )]))

(define (compile-case-cl c case-end c1)
  (match c
    [(Clause datums b)  (let ((meets-clause (gensym 'meetsClause))
                              (fails-clause (gensym 'failsClause)))
                      (seq  
                          (compile-case-datum datums meets-clause fails-clause c1)
                          (Label meets-clause)
                          (compile-e b c1) ; puts val of case clause expr in rax
                          (Jmp case-end)
                          (Label fails-clause)
                          ; If here, the value of rax should be the target val
                          ))]
      [_                  (error "Not a clause")]))


; Basically running a cond on if any of the datum match the target expression
(define (compile-case-datum dtm meets-clause fails-clause c)
  (match dtm
    [(cons d ds)   (let ((d1 (gensym 'endDatum)))
                      (seq  (Push rax) ; rax should contain the value of the expression to be matched, so store it in the stack
                            (compile-e d c) ; now compiled d is in rax
                            (Pop rbx) ; bring back the target expr
                            (Cmp rax rbx)
                            (Mov rax rbx) 
                            (Je meets-clause) ; Jumps to meets-clause if a match
                            (Label d1) ; Here to make debugging earlier
                            (compile-case-datum ds meets-clause fails-clause c)))] ; Moves onto the other datum to check
    ['() (seq (Jmp fails-clause))]))