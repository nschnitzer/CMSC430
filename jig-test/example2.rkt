#lang racket

(define (len xs)
  (if (empty? xs)
    0
    (add1 (len (cdr xs)))))

(define (len2 xs)
  (len2TC 0 xs))

(define (len2TC acc xs)
  (if (empty? xs)
    acc
    (len2TC (add1 acc) (cdr xs))))


