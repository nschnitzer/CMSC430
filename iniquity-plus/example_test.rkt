#lang racket

(define (list . xs) xs)

(define (f x y . xs)
  (list x y xs))

(f 1 2)
