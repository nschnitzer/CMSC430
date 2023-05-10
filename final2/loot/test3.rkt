#lang racket
(define (even? x)
  (if (zero? x)
      #t
      (odd? (sub1 x))))

(define (odd? x)
  (if (zero? x)
      #f
      (even? (sub1 x))))

(with-handlers
    ([even?   (lambda (x) "even")]
     [odd?    (lambda (x) "odd")])
    (raise 22))
