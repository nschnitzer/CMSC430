#lang racket

(let ((a 0)) (with-handlers ([zero? (lambda (x) (+ a x))]) (raise a)))
