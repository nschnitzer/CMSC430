#lang racket

(with-handlers ([string? (lambda (x) (cons "got" x))])
               (raise "x"))

