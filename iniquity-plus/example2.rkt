#lang racket

(define f
  (case-lambda
    [x #f]
    [y #t]))

(cons (f 1) (cons (f 1 2) '()))
