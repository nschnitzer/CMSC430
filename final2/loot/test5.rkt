#lang racket

(with-handlers ([cons? (lambda (x) x)]
                [string? (lambda (x) (cons "got string" x))])
  (with-handlers ([vector? (lambda (x) (cons "vector???" x))])
    (raise "x")))