#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax) ; return
(define rdi 'rdi) ; arg
(define r9  'r9)  ; scratch

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq (Call 'read_byte))]
    ['peek-byte (seq (Call 'peek_byte))]))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    ['zero?
     (seq (assert-integer rax)
          (Cmp rax 0)
          (if-equal))]
    ['char?
     (type-pred mask-char type-char)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object? (eq-value eof)]
    ['write-byte
     (seq (assert-byte)
          (Mov rdi rax)
          (Call 'write_byte)
          (Mov rax (value->bits (void))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'err))))

(define (type-pred mask type)
  (seq (And rax mask)
       (Cmp rax type)
       (if-equal)))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))

; Bits Bits Register -> asm
; Basically the same as assert-type except it
; raises an error if the types match
(define (assert-bool-helper mask type reg)
  (seq  (Mov r9 reg)
        (And r9 mask)
        (Cmp r9 type)
        (Je  'err)))

; Register -> asm
; Checks if the value in the register is a Boolean. Raises error if not.
; First checks if int or char since they have bit masks.
; Then compares the bits directly to the 2 boolean values.
(define (assert-bool reg)
  (let ((goodBool (gensym 'assertBool)))
    (seq  (assert-bool-helper mask-int type-int reg)
          (assert-bool-helper mask-char type-char reg)
          (Cmp reg val-true)
          (Je goodBool)
          (Cmp reg val-false)
          (Je goodBool)
          (Jmp 'err)
          (Label goodBool))))

(define (assert-codepoint)
  (let ((ok (gensym)))
    (seq (assert-integer rax)
         (Cmp rax (value->bits 0))
         (Jl 'err)
         (Cmp rax (value->bits 1114111))
         (Jg 'err)
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))

(define (assert-byte)
  (seq (assert-integer rax)
       (Cmp rax (value->bits 0))
       (Jl 'err)
       (Cmp rax (value->bits 255))
       (Jg 'err)))

;; -> Asm
;; set rax to #t or #f if comparison flag is equal
(define (if-equal)
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))

;; Value -> Asm
(define (eq-value v)
  (seq (Cmp rax (value->bits v))
       (if-equal)))
