#lang racket

(with-handlers
    ([zero?  (lambda (x) x)])
  (raise "x"))
