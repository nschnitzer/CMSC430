#lang racket

(define (print-alphabet i)
  (if (zero? i)
      (void)
      (begin (print-alphabet (sub1 i))
             (write-byte (- 123 i)))))

(print-alphabet 3)
;;; (write-byte 123)