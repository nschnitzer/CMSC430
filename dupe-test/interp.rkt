#lang racket
(provide interp)
(require "ast.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Int i) i]
    [(Prim1 op e) (interp-prim op (interp e))]
    [(Bool b) b]
    [(If e1 e2 e3)
       (let ((interp_p (interp e1)))
         (if (or (eq? interp_p #f) (eq? interp_p 0))
             (interp e3)
             (interp e2)))]))

(define (interp-prim op i)
  (match op
    ['add1 (+ i 1)]
    ['sub1 (- i 1)]))


