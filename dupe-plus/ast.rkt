#lang racket
(provide Int Bool Prim1 If Cond Case Clause)

;; TODO: Add other expression forms to the type and structure definitions.

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim1 Op Expr)
;; | (If Expr Expr Expr)
;;
;; type Op = 'add1 | 'sub1 | 'zero? | abs | - | not
; type Datum = Integer | Boolean

; | (Cond [Listof CondClause] Expr)
; | (Case Expr [Listof CaseClause] Expr)
; type CondClause = (Clause Expr Expr)
; type CaseClause = (Clause [Listof Datum] Expr)

(struct Int (i)         #:prefab)
(struct Bool (b)        #:prefab)
(struct Prim1 (p e)     #:prefab)
(struct If (e1 e2 e3)   #:prefab)
(struct Cond (cs e)     #:prefab)
(struct Case (e cs el)  #:prefab)
(struct Clause (p b)    #:prefab)