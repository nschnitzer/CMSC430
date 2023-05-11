#lang racket
(let ((f (let ((x 42))
             (lambda (y) (+ y x)))))
      (f 42))
