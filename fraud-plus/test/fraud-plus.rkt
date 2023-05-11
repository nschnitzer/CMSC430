#lang racket
(require "../parse.rkt"
         "../interp.rkt"
         "../compile.rkt"
         "../types.rkt"
         a86/interp
         rackunit)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(define (run-interp e)
  (interp (parse e)))

(define (run-compile e)
  (bits->value (asm-interp (compile (parse e)))))

(define (test run)
  ;; Fraud+ examples
  (check-equal? (run '(integer? 7)) #t)
  (check-equal? (run '(integer? #f)) #f)
  (check-equal? (run '(boolean? 7)) #f)
  (check-equal? (run '(boolean? #f)) #t)
  (check-equal? (run '(boolean? #t)) #t)
  (check-equal? (run '(let () 7)) 7)
  (check-equal? (run '(let ((x 1)) x)) 1)
  (check-equal? (run '(let* () 7)) 7)
  (check-equal? (run '(let* ((x 1)) x)) 1)
  (check-equal? (run '(let ((x 1) (y 2)) (+ x y))) 3)
  (check-equal? (run '(let ((x 1))
                        (let* ((x 2) (x x)) x)))
                2)
  (check-equal? (run '(+)) 0)
  (check-equal? (run '(+ 5)) 5)
  (check-equal? (run '(+ 1 2 3)) 6))

(test run-interp)
(test run-compile)
