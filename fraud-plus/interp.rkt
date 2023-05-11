#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 (ext r #f v1))
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive
    [(PrimN p es) 
      (match (interp*-env es r)
        ['err 'err]
        [vs (interp-primN p vs)])
    ]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs e) (interp-cond cs (interp-env e r) r)]
    ;; TODO: implement case
    [(Case ev cs el) (interp-case (interp-env ev r) cs (interp-env el r) r)]
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    ;;; [(Let (list x) (list e1) e2)
    ;;;  (match (interp-env e1 r)
    ;;;    ['err 'err]
    ;;;    [v (interp-env e2 (ext r x v))])]
    [(Let x e1 e2)
      (interp-let x (interp*-env e1 r) e2 r)]
    ;; TODO: implement let*
    [(Let* xs es e) (interp-let* xs es e r)]))

(define (interp-let vs e1 e2 r)
  (match e1
    [(cons x xs) (interp-let (cdr vs) xs e2 (ext r (first vs) x))]
    ['() (interp-env e2 r)]
    ['err 'err]
  ))

;;; (define (interp-let vs e1 e2 r)
;;;   e1)

(define (interp-let* vs e1 e2 r)
  (match e1
    [(cons x xs)  (interp-let* (cdr vs) xs e2 (ext r (first vs) (interp-env x r)))]
    ['()  (interp-env e2 r)]
    ['err 'err]
  )
)
;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]
    ['() 'err]))

;;; (define (lookup r x)
;;;   (print r))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

(define (interp-cond x e r)
  (match x
    [(cons c cs)  (let ((cl (interp-cond-clause c r)))
                      (if (first cl)
                          (second cl)
                          (interp-cond cs e r)))]
    ['() e]))

(define (interp-cond-clause c r)
  (match c
    [(Clause p rx)  (cons (interp-env p r) (cons (interp-env rx r) '()))]
    ['() '()]))

(define (interp-case e cs el r)
  (match cs
    [(cons x xs)    (let ((cls (interp-case-clause x r)))
                        (if (eq? (memq e (car cls)) #f)
                            (interp-case e xs el r)
                            (second cls)))]
    ['()            el]
  ))

(define (interp-case-clause c r)
  (match c
    [(Clause cs exp) (cons (interp*-env cs r)
                            (cons (interp-env exp r)
                                  '()))]
    [_ '()])) ;; should never get here
