#lang racket
(provide interp interp-env)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (vector Value ...)
;; | (string Char ...)
;; | (Value ... -> Answer)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (interp-env e '() ds '())]))

;; Expr Env Defns Handlers -> Answer
(define (interp-env e r ds hs)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (interp-var x r ds hs)]
    [(Str s)  (string-copy s)]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match (interp-env e r ds hs)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds hs)
       ['err 'err]
       [v1 (match (interp-env e2 r ds hs)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds hs)
       ['err 'err]
       [v1 (match (interp-env e2 r ds hs)
             ['err 'err]
             [v2 (match (interp-env e3 r ds hs)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
    [(If p e1 e2)
     (match (interp-env p r ds hs)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds hs)
            (interp-env e2 r ds hs))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds hs)
       ['err 'err]
       [_    (interp-env e2 r ds hs)])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds hs)
       ['err 'err]
       [v (interp-env e2 (ext r x v) ds hs)])]
    [(Lam _ xs e)
     (λ vs
       ; check arity matches
       (if (= (length xs) (length vs))
           (interp-env e (append (zip xs vs) r) ds hs)
           'err))]
    [(App e es)
     (match (interp-env e r ds hs)
       ['err 'err]
       [f
        (match (interp-env* es r ds hs)
          ['err 'err]
          [vs
           (if (procedure? f)
               (apply f vs)
               'err)])])]
    [(Match e ps es)
     (match (interp-env e r ds hs)
       ['err 'err]
       [v
        (interp-match v ps es r ds hs)])]
    [(Raise e) (catch-error (interp-env e r ds hs) hs)]


    [(ExcMgr hdlrs e)
     (interp-env e r ds (append (interp-handlers hdlrs r ds hs) hs))]
    ;; Basically going to parse the different components of each
    ;; handler and then append them to hs (maybe reverse?)
    ;; then call interp-env on e w/ the new handlers
    ;; The handlers should be popped off hdlrs after done
    [(Handler pred func)
     (match (interp-env pred r ds hs)
       ['err  'err]
       [p  (match (interp-env func r ds hs)
             ['err 'err]
             [f  (cons p (cons f '()))])])]
    ))

(define (interp-handlers hdlrs r ds hs)
  (match hdlrs
    [(cons x xs) (interp-env* hdlrs r ds hs)]))


(define (use-handler e h)
  (match h
    [(cons p (cons f '()))  (let ((res (if (procedure? p)
                                (apply p (cons e '()))
                                'err)))
                   (if (eq? res 'err)
                       'err
                       (if res (apply f (cons e '())) #f)))]
    [_ 'err_handler]))

(define (catch-error e hs)
  (match hs
    [(cons x xs) (let ((res (use-handler e x)))
                   (if res 
                       res 
                       (catch-error e xs)))]
    [_  'err_catch-error]))

;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds hs)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r hs)
       [#f (interp-match v ps es r ds hs)]
       [r  (interp-env e r ds hs)])]))

;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r hs)
  (match p
    [(PWild) r]
    [(PVar x) (ext r x v)]
    [(PLit l) (and (eqv? l v) r)]
    [(PBox p)
     (match v
       [(box v)
        (interp-match-pat p v r hs)]
       [_ #f])]
    [(PCons p1 p2)
     (match v
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r hs)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1 hs)])]
       [_ #f])]
    [(PAnd p1 p2)
     (match (interp-match-pat p1 v r hs)
       [#f #f]
       [r1 (interp-match-pat p2 v r1 hs)])]))

;; Id Env [Listof Defn] -> Answer
(define (interp-var x r ds hs)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f xs e) (interp-env (Lam f xs e) '() ds hs)]
            [#f 'err])]
    [v v]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds hs)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds hs)
       ['err 'err]
       [v (match (interp-env* es r ds hs)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Defns Symbol -> [Maybe Defn]
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
