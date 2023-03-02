#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? exact-integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(list (? op1? o) e) (Prim1 o (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(cons 'cond tl) (Cond (parse-cond-clauses tl) (parse-else tl))]
    [(cons 'case (cons ex cs)) (Case (parse ex) (parse-case-clauses cs) (parse-else cs))]
    [_ (error "Parse error")]))

;; Any -> Boolean
(define (op1? x)
  (memq x '(add1 sub1 zero? abs not -)))

(define (is-else-clause? c)
  (if (eq? (memq (car c) '(else)) #f) #f #t))

(define (parse-cond-clause c)
  (Clause (parse (car c)) (parse (second c)))
)

(define (parse-else xs)
  (match xs
    [(cons c cs) (if (is-else-clause? c)
                     (parse (second c))
                     (parse-else cs))]
    ['() (error "Parse error")]))

;; [Listof CondClause]
(define (parse-cond-clauses x)
  (match x
    [(cons c cs) (if (is-else-clause? c)
                     '()
                     (cons (parse-cond-clause c) 
                           (parse-cond-clauses cs)))]
    ['() '()]))


;; Parses a Clause for a case structure
;; should receive something in form of [(listof Datum) expr]
;; parses through the list of datums using a map
;; returns Clause (listof Datum) expr
(define (parse-case-clause c)
  (Clause (map (lambda (x) (parse x)) (car c)) (parse (second c))))

;; Parses a list of Case Clauses for a case structure
;; should receive simething in the form of (listof [(listof Datum) expr])
;; Need to parse through all of the non else clauses and append them to a list
(define (parse-case-clauses x)
  (match x
    [(cons c cs)  (if (is-else-clause? c)
                      '()
                      (cons (parse-case-clause c) 
                            (parse-case-clauses cs)))]
    ['() '()]))