#lang racket
(provide parse parse-define parse-e op? op1 op2 is-pred?)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match s
    [(cons (and (cons 'define _) d) s)
     (match (parse s)
       [(Prog ds e)
        (Prog (cons (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn f xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? exact-integer?)            (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? string?)                   (Str s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]
    [(list (or 'lambda 'λ) xs e)
     (if (and (list? xs)
              (andmap symbol? xs))
         (Lam (gensym 'lambda) xs (parse-e e))
         (error "parse lambda error"))]
    [(list 'with-handlers '() e) (ExcMgr '() (parse-e e))]
    [(list 'with-handlers hdlrs e)
     (ExcMgr (parse-handlers hdlrs) (parse-e e))]

    [(list 'raise e) (Raise (parse-e e))]

    [(cons e es)
     (App (parse-e e) (map parse-e es))]
    [_ (error "Parse error" s)]))

(define (parse-handlers hdlrs)
  (match hdlrs
    [(cons x xs) (cons (parse-handler x) (parse-handlers xs))]
    ['()         '()]))

(define (parse-handler hdlr)
  (match hdlr
    [(list x y)
     (Handler (parse-predicate x) (parse-e y))]
    [_  (error "error parsing handler")]))

(define (parse-predicate p)
  (match p
    [(list (or 'lambda 'λ) xs e) ;; Accepts a lambda
     (if (and (list? xs)
              (andmap symbol? xs))
         (Lam (gensym 'lambdaPreicated) xs (parse-e e))
         (error "parse lambda in parse-predicate error"))]

    [(? (op? op1) p) (if (is-pred? p) ;; using a predicate
           (let ((lamName (gensym 'lambdaPredicate))
                 (pname   (gensym 'lambdaPredicate)))
             (Lam lamName (list pname) (Prim1 p (Var pname))))
           
           (error "Not a valid predicate... "))]
    [(? (op? op0)) (error "Not a valid predicate...1")]
    [(? (op? op2)) (error "Not a valid predicate...2")]
    [(? symbol?) (let ((lamName (gensym 'lambdaPredicate))
                       (pname   (gensym 'lambdaPredicate)))
                   (Lam lamName (list pname) (App (Var p) (Var pname))))] ;; if its calling a function

    [_ (error "not a valid predicate...3")]
    ))

(define (parse-match e ms)
  (match ms
    ['() (Match e '() '())]
    [(cons (list p r) ms)
     (match (parse-match e ms)
       [(Match e ps es)
        (Match e
               (cons (parse-pat p) ps)
               (cons (parse-e r) es))])]))

(define (parse-pat p)
  (match p
    [(? boolean?) (PLit p)]
    [(? exact-integer?) (PLit p)]
    [(? char?)    (PLit p)]
    ['_           (PWild)]
    [(? symbol?)  (PVar p)]
    [(list 'quote (list))
     (PLit '())]
    [(list 'box p)
     (PBox (parse-pat p))]
    [(list 'cons p1 p2)
     (PCons (parse-pat p1) (parse-pat p2))]
    [(list 'and p1 p2)
     (PAnd (parse-pat p1) (parse-pat p2))]))

(define op0
  '(read-byte peek-byte void))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref))

(define preds
  '(zero? char? eof-object? empty? cons? box? string? vector?))

(define (is-pred? x)
  (if (eq? (memq x preds) #f) #f #t))


(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))