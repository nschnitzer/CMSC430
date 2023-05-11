#lang racket

(define (g x)
  x)

(define (f x y)
  (begin (g 42)
         84))

(f 42 42)
