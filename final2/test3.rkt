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
    [even?   "even"]
    [odd?    "odd"]
  (raise 22))