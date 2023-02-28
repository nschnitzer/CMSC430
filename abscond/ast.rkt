#lang racket

(provide Int)

; data Expr = Int of int
(struct Int (i) #:prefab)

