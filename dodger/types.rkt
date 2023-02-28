#lang racket
(provide (all-defined-out))
 
(define int-shift     1)
(define char-shift    2)
(define type-int    #b0)
(define type-char  #b01)
(define mask-char  #b11)
(define val-true  #b011)
(define val-false #b111)
 
(define (bits->value b)
  (cond [(= type-int (bitwise-and b #b1))         
         (arithmetic-shift b (- int-shift))]
        [(= type-char (bitwise-and b #b11))
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [else (error "invalid bits")]))
 
(define (value->bits v)
  (cond [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]))

;; Everytime we add another datatype, we reduce the number of integers we can represent by half
;; if we represent if it is a datatype as its own bit 
;; We avoid this by saying if it ends in 0 it is an int, otherwise it is not an int. Further masking is required
