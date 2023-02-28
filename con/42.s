        default rel
        section .text
        global _entry
_entry:
        mov rax, 0
        cmp rax, 0
        jne _els3368
        mov rax, 42
        jmp _if3367
_els3368:
        mov rax, 10
        add rax, 1
_if3367:
        ret
