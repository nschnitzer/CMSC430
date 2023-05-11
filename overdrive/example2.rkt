#lang racket
(let ((v 1)) ((if (zero? v) 
     (lambda (x) x) 
     (lambda (x) (+ x 42))) 42))
