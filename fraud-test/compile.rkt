#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

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
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(LetVamp x e1 e2) (compile-let-vamp x e1 e2 c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (match (lookup x c)
    [(cons 'vamp (cons i '())) (seq (Mov rax (Offset rsp i)))]
    [i              (seq (Mov rax (Offset rsp i)))]))
  ;;; (let ((i (lookup x c)))
  ;;;   (seq (Mov rax (Offset rsp i)))))

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

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax (value->bits #f))
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
;;; (define (compile-let x e1 e2 c)
;;;   (seq (compile-e e1 c)
;;;        (Push rax)
;;;        (compile-e e2 (cons x c))
;;;        (Add rsp 8))) ;; Moves the stack pointer over

;; Compiles cleanly, but runtime error
;;; (define (compile-let x e1 e2 c)
;;;   (match (lookup x c)
;;;     [(cons 'vamp _) (Jmp 'raise_error_align)]
;;;     [_              (seq (compile-e e1 c)
;;;                          (Push rax)
;;;                          (compile-e e2 (cons x c))
;;;                          (Add rsp 8))]
;;;     )
;;;   )
(define (compile-let x e1 e2 c)
  (match (lookup x c)
    [(cons 'vamp _) (error "Cannot reassign variable: " x)]
    [_              (seq (compile-e e1 c)
                         (Push rax)
                         (compile-e e2 (cons x c))
                         (Add rsp 8))]))



;; Id Expr Expr CEnv -> Asm
(define (compile-let-vamp x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons (cons 'vamp x) c))
       (Add rsp 8))
  )

;; Id CEnv -> Integer
;;; (define (lookup x cenv)
;;;   (match cenv
;;;     ['() (error "undefined variable:" x)]
;;;     [(cons y rest)
;;;      (match (eq? x y)
;;;        [#t 0]
;;;        [#f (+ 8 (lookup x rest))])])) ;; + 8 b/c it needs to move the stack pointer over

(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons (cons 'vamp y) rest)
     (match (eq? x y)
       [#t (cons 'vamp (cons 0 '()))]
       [#f (match (lookup x rest)
             [(cons 'vamp x)  (cons 'vamp (+ 8 x))]
             [x               (+ 8 x)]
             )]
       )
     ]
    [(cons y rest)
      (match (eq? x y)
        [#t 0]
        [#f (match (lookup x rest)
              [(cons 'vamp (cons x '())) (cons 'vamp (cons (+ 8 x) '()))]
              [x              (+ 8 x)]
              )]
      )]
  )
)
