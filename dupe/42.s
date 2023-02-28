        default rel
        section .text
        global _entry
_entry:
        mov rax, 1
        cmp rax, 3
        je _if4823
        mov rax, 84
        jmp _if4824
_if4823:
        mov rax, 2
_if4824:
        ret
