        default rel
        section .text
        extern _peek_byte
        extern _read_byte
        extern _write_byte
        extern _raise_error
        global _entry
_entry:
        mov rax, 84
        push rax
        mov rax, 168
        push rax
        mov rax, [rsp + 8]
        push rax
        mov rax, [rsp + 8]
        pop r8
        mov r9, r8
        and r9, 1
        cmp r9, 0
        jne _raise_error_align
        mov r9, rax
        and r9, 1
        cmp r9, 0
        jne _raise_error_align
        add rax, r8
        add rsp, 8
        add rsp, 8
        ret
_raise_error_align:
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        call _raise_error
