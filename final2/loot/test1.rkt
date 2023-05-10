#lang racket

(with-handlers
    ([zero?  (lambda (x) x)])
    (raise 0))
