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
  ;; Abscond examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8)

  ;; Blackmail examples
  (check-equal? (run '(add1 (add1 7))) 9)
  (check-equal? (run '(add1 (sub1 7))) 7)

  ;; Con examples
  (check-equal? (run '(if (zero? 0) 1 2)) 1)
  (check-equal? (run '(if (zero? 1) 1 2)) 2)
  (check-equal? (run '(if (zero? -7) 1 2)) 2)
  (check-equal? (run '(if (zero? 0)
                          (if (zero? 1) 1 2)
                          7))
                2)
  (check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                          (if (zero? 1) 1 2)
                          7))
                7)

  ;; Con+ examples
  (check-equal? (run '(abs 10)) 10)
  (check-equal? (run '(abs -10)) 10)
  (check-equal? (run '(- (abs -10))) -10)
  (check-equal? (run '(- 10)) -10)
  (check-equal? (run '(- -10)) 10)
  (check-equal? (run '(- (- 10))) 10)
  (check-equal? (run '(cond [else 5])) 5)
  (check-equal? (run '(cond [else (- 10)])) -10)
  (check-equal? (run '(cond [(zero? 1) 2] [else 3])) 3)
  (check-equal? (run '(cond [(zero? 0) 2] [else 3])) 2)
  (check-equal? (run '(cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3])) 4)
  (check-equal? (run '(cond [(zero? 1) 1] [(zero? 2) 2] [(zero? 3) 3] [(zero? 4) 4] [(zero? 5) 5] [else 6])) 6)

  ;; Dupe examples
  (check-equal? (run #t) #t)
  (check-equal? (run #f) #f)
  (check-equal? (run '(if (zero? 0)
                          (if (zero? 1) #t #f)
                          7))
                #f)

  ;; Dupe+ exampels
  (check-equal? (run '(not #t)) #f)
  (check-equal? (run '(not #f)) #t)
  (check-equal? (run '(not 7)) #f)
  (check-equal? (run '(cond [else #t])) #t)
  (check-equal? (run '(cond [(not #t) 2] [else 3])) 3)
  (check-equal? (run '(cond [(if #t #t #f) 2] [else 3])) 2)
  (check-equal? (run '(cond [(zero? 1) 2] [(if (not (zero? (sub1 2))) #t #f) 4] [else 3])) 4)
  (check-equal? (run '(cond [#t 1] [else 2])) 1)
  (check-equal? (run '(cond [1 1] [else 2])) 1)

  (check-equal? (run '(case 2 [else 1])) 1)
  (check-equal? (run '(case 2 [() 3] [else 1])) 1)
  (check-equal? (run '(case 2 [(2) 3] [else 1])) 3)
  (check-equal? (run '(case 4 [(2) 3] [else 1])) 1)
  (check-equal? (run '(case 2 [(7 2) 3] [else 1])) 3)
  (check-equal? (run '(case 4 [(7 2) 3] [else 1])) 1)
  (check-equal? (run '(case 2 [(7 2 #t) 3] [else 1])) 3)
  (check-equal? (run '(case 4 [(7 2 #t) 3] [else 1])) 1)
  (check-equal? (run '(case #t [(7 2 #t) 3] [else 1])) 3)
  (check-equal? (run '(case #f [(7 2 #t) 3] [else 1])) 1)
  )

(test run-interp)
(test run-compile)
