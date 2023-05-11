#lang racket
(provide (all-defined-out))

;; type Expr =
;; | (Eof)
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Prim0 Op0)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (PrimN OpN [Listof Expr])
;; | (If Expr Expr Expr)
;; | (Begin Expr Expr)
;; | (Let  [Listof Id] [Listof Expr] Expr)  ; lengths must be equal
;; | (Let* [Listof Id] [Listof Expr] Expr)  ; lengths must be equal
;; | (Var Id)
;; | (Cond [Listof CondClause] Expr)
;; | (Case Expr [Listof CaseClause] Expr)
;; type Id  = Symbol
;; type Op0 = 'read-byte
;; type Op1 = 'add1 | 'sub1 | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
;;          | 'integer? | 'boolean?
;; type Op2 = '-
;; type OpN = '+
;; type CondClause = (Clause Expr Expr)
;; type CaseClause = (Clause [Listof Datum] Expr)
;; type Datum = Integer | Boolean | Character

(struct Eof   ()         #:prefab)
(struct Int   (i)        #:prefab)
(struct Bool  (b)        #:prefab)
(struct Char  (c)        #:prefab)
(struct Prim0 (p)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct Prim2 (p e1 e2)  #:prefab)
(struct PrimN (p es)     #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct Begin (e1 e2)    #:prefab)
(struct Let   (xs es e)  #:prefab)
(struct Let*  (xs es e)  #:prefab)
(struct Var   (x)        #:prefab)
(struct Cond (cs e)      #:prefab)
(struct Clause (p body)  #:prefab)
(struct Case (e cs el)   #:prefab)
