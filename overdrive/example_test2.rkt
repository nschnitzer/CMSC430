#lang racket

(let ((x 97))
  (lambda (gemsym 'lambda) (x)) (read-byte))
