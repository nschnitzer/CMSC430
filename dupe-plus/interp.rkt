#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    [(Cond cs e) (interp-cond cs (interp e))]
    [(Case e cs el) (interp-case (interp e) cs (interp el))]
    ))

(define (interp-cond-clause c)
  (match c
    [(Clause p r)  (cons (interp p) (cons (interp r) '()))]
    ['() '()]))

(define (interp-cond x e)
  (match x
    [(cons c cs)  (let ((cl (interp-cond-clause c)))
                      (if (first cl)
                          (second cl)
                          (interp-cond cs e)))]
    ['() e]))


(define (interp-case e cs el)
  (match cs
    [(cons x xs)    (let ((cls (interp-case-clause x)))
                        (if (eq? (memq e (car cls)) #f)
                            (interp-case e xs el)
                            (second cls)))]
    ['()            el]
  ))

(define (interp-case-clause c)
  (match c
    [(Clause cs exp) (cons (map (lambda (x) (interp x)) cs)
                            (cons (interp exp)
                                  '()))]
    [_ '()])) ;; should never get here