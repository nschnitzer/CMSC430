#lang racket

(with-handlers ([zero? (lambda (x) (cons "got number" x))]
                [string? (lambda (x) (cons "got string" x))])
               (raise 0))

