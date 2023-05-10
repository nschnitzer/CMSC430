#lang racket

(with-handlers ([string? (lambda (x) (cons "got" (cons x "first")))])
  (with-handlers ([string? (lambda (x) (cons "got" (cons x "second")))])
    (if #t (raise "here") "idk")))