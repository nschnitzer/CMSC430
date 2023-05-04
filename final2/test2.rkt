#lang racket

(with-handlers 
    [(lambda (x) (if (even? x) #t #f)) "even"]
  (raise 22))