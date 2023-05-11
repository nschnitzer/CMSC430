#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r10 'r10) ; for holding the num of args
(define r8  'r8)  ; scratch

;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Push rbx)
           (Push r15)
           (Push r10)
           (Push r8)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
           (Pop r8)
           (Pop r10)
           (Pop r15)
           (Pop rbx)
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           (Or rsp 8)
           (Jmp 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f fun)
     (compile-fun f fun)]))

;; Id Fun -> Asm
(define (compile-fun f fun)
  (match fun
    [(FunPlain xs e)
     (seq (Label (symbol->label f))
          ;; TODO: check arity
          (Mov rax (length xs))
          (Cmp r10 rax)
          (Jne 'raise_error_align)
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs)))
          (Ret))]
    ;; TODO: handle other kinds of functions
    [(FunRest xs x e)
     (let ((loop-begin (gensym 'funrest))
           (loop-end   (gensym 'funrest)))
       (seq (Label (symbol->label f))
            (Mov rax (length xs))
            (Cmp r10 rax)
            (Jl 'raise_error_align)
            (Mov 'r8 r10)
            (Sub 'r8 rax)
            (Mov rax val-empty)

            (Label loop-begin)
            (Cmp 'r8 0)
            (Je loop-end)
            (Sub 'r8 1)
            (Mov (Offset rbx 0) rax)
            (Pop rax)
            (Mov (Offset rbx 8) rax)
            (Mov rax rbx)
            (Or rax type-cons)
            (Add rbx 16)
            (Jmp loop-begin)
            (Label loop-end)

            (Push rax)
            (compile-e e (cons x (reverse xs)))
            (Add rsp (* 8 (+ (length xs) 1)))
            ;;; (Pop 'r8)
            (Ret)
            ))]
    [(FunCase cs)
     (seq (Label (symbol->label f))
          (funCaseAux cs)
          (Ret))]
    [_
     (seq)]))

(define (funCaseAux cs)
  (match cs
    [(cons (FunPlain xs e) lst) 
      (let ((plain (gensym 'CasePlain)))
      (seq  (Mov rax (length xs))
            (Cmp r10 rax)
            (Je plain)
            (funCaseAux lst)
            (Ret)
            (Label plain)
            (compile-e e (reverse xs))
            (Add rsp (* 8 (length xs)))
            (Ret)
            ))]
    [(cons (FunRest xs x e) lst)
      (let ((rest            (gensym 'CaseRest))
            (rest-loop-begin (gensym 'CaseRest))
            (rest-loop-end   (gensym 'CaseRest)))
      (seq  (Mov rax (length xs))
            (Cmp r10 rax)
            (Jge rest)
            (funCaseAux lst)
            (Ret)
            
            (Label rest)
            (Mov r8 r10)
            (Sub r8 rax)
            (Mov rax val-empty)

            (Label rest-loop-begin)
            
            (Cmp r8 0)
            (Je rest-loop-end)
            
            (Sub r8 1)
            (Mov (Offset rbx 0) rax)
            (Pop rax)
            (Mov (Offset rbx 8) rax)
            (Mov rax rbx)
            (Or rax type-cons)
            (Add rbx 16)
            (Jmp rest-loop-begin)

            (Label rest-loop-end)
            
            (Push rax)
            (compile-e e (cons x (reverse xs)))
            (Add rsp (* 8 (+ 1 (length xs))))
            (Ret)))]
    [_
      (Jmp 'raise_error_align)]))

;; Function not used
;; leaving here because I have no idea why it didn't
;; work. It is basically the same thing that is the loop
;; in the funRest compile code... It was just supposed to separate
;; it to be more readable...
(define (pop-n)
  (let ((popn (gensym 'restargs))
        (no-more (gensym 'restargs)))
    (seq (Label popn)
         (Push rax)
         (Mov rax 1)
         (Cmp 'r8 rax)
         (Pop rax)
         (Jle no-more)
         (Mov (Offset rbx 0) rax)
         (Pop rax)
         (Mov (Offset rbx 8) rax)
         (Mov rax rbx)
         (Or rax type-cons)
         (Add rbx 16)

         (Sub 'r8 1)
         (Jmp popn))
         (Label no-more) 
    ))


;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(App f es)         (compile-app f es c)]
    [(Apply f es e)     (compile-apply f es e c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

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

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (compile-op3 p)))

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
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         ;; TODO: communicate argument count to called function
         (Mov r10 (length es))
         (Jmp (symbol->label f))
         (Label r))))

;; Id [Listof Expr] Expr CEnv -> Asm
;; es are the args passed to f first
;; ex: apply f x y (list 1 2)
;;      es -> [x, y]
(define (compile-apply f es e c)
  ;; TODO: implement apply
  (let ((r          (gensym 'ret))
        (begin-loop (gensym 'apply))
        (end-loop   (gensym 'apply)))
    (seq  (Lea rax r)
          (Push rax) ;; Put return address above args
          (Mov r10 (length es)) ;; Need counter for args

          (compile-es es (cons #f c)) ;; compile first args
          (compile-e e (append es (cons #f c))) ;; Need to use append since es is list

          ;; pop elements off the list and give to function
          (Label begin-loop)
          (Cmp rax val-empty)
          (Je end-loop) ; empty list
          (Xor rax type-cons)
          (Mov r8 (Offset rax 8)) ;; car
          (Push r8)
          (Mov rax (Offset rax 0)) ;; cdr
          (Add r10 1)
          (Jmp begin-loop)
          (Label end-loop)
          (Jmp (symbol->label f))
          (Label r))))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
          (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
