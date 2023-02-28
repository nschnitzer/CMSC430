#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? exact-integer?) (Int s)]
    [(? boolean?) (Bool s)]
    ;; TODO: Handle abs, -, and not
    [(list (? op1? o) e) (Prim1 o (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    ;; TODO: Handle cond
    [(cons 'cond tl) (Cond (parse-cond-clauses tl) (parse-else tl))]
    ;;; [(cons 'cond tl) (is-else-clause? (car tl))]
    ;; TODO: Handle case
    ;; TODO: Remove the first wildcard clause (on the next line) after you've
    ;; added clauses for parsing cond and case. It's just here so running the
    ;; test suite doesn't trigger parse errors.
    [_ (Int 0)]
    [_ (error "Parse error")]))

;; Any -> Boolean
(define (op1? x)
  (memq x '(add1 sub1 zero? abs not -)))

(define (is-else-clause? c)
  (if (eq? (memq (car c) '(else)) #f) #f #t))

(define (parse-cond-clause c)
  (Clause (parse (car c)) (parse (second c)))
)

(define (parse-else cs)
  (if (is-else-clause? (car cs))
      (parse (second (car cs)))
      (parse-else (cdr cs))))

;; [Listof CondClause]
(define (parse-cond-clauses cs)
  (if (is-else-clause? (car cs))
    (list )
  ;;;   [_    (cons (Clause (parse (car (car cs))) 
  ;;;                       (parse (cdr (car cs)))) 
  ;;;               (parse-cond-clauses (cdr cs)))]
  ;;; ))
    (cons (parse-cond-clause (car cs)) (parse-cond-clauses (cdr cs)))))