#lang racket
(provide main)
(require "parse.rkt" "interp.rkt")

;; -> void
;; Parse and interpret the contents of stdin
;; print results on stdout
(define (main)
  (read-line) ; ignore racket line
  (println (interp (parse (read))))
  )

