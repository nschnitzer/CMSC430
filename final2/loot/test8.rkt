#lang racket

(define (f x) x)

(with-handlers ([(lambda (x) (zero? x)) (lambda (x) (f x))])
               (raise 0))

