#lang racket

(provide main)
(require "parse.rkt" "compiler.rkt" a86/printer)

;; -> void
;; Compile contents of stdin
;; emit asm code on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (asm-display (compile (parse (read)))))


