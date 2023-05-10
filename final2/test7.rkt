#lang racket

(define (f x) (add1 x))

(with-handlers ([string? (lambda (x) (add1 4))])
               (raise "x"))

