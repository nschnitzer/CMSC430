        default rel
        section .text
        extern _peek_byte
        extern _read_byte
        extern _write_byte
        extern _raise_error
        global _entry
_entry:
        push rbx
        mov rbx, rdi
        lea rax, [rel _ret4954]
        push rax
        mov rax, 48
        push rax
        jmp _label_print_alphabet_fce60b92d75b66f
_ret4954:
        pop rbx
        ret
_label_print_alphabet_fce60b92d75b66f:
        mov rax, [rsp + 0]
        mov r9, rax
        and r9, 15
        cmp r9, 0
        jne _raise_error_align
        cmp rax, 0
        mov rax, 56
        mov r9, 24
        cmove rax, r9
        cmp rax, 56
        je _if4955
        mov rax, 120
        jmp _if4956
_if4955:
        mov rax, [rsp + 0]
        mov r9, rax
        and r9, 15
        cmp r9, 0
        jne _raise_error_align
        sub rax, 16
        push rax
        mov r8, [rsp + 0]
        mov [rsp + 8], r8
        add rsp, 8
        jmp _label_print_alphabet_fce60b92d75b66f
        mov rax, 1968
        push rax
        mov rax, [rsp + 8]
        pop r8
        mov r9, r8
        and r9, 15
        cmp r9, 0
        jne _raise_error_align
        mov r9, rax
        and r9, 15
        cmp r9, 0
        jne _raise_error_align
        sub r8, rax
        mov rax, r8
        mov r9, rax
        and r9, 15
        cmp r9, 0
        jne _raise_error_align
        cmp rax, 0
        jl _raise_error_align
        cmp rax, 4080
        jg _raise_error_align
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        mov rdi, rax
        call _write_byte
        add rsp, r15
        mov rax, 120
_if4956:
        add rsp, 8
        ret
_raise_error_align:
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        call _raise_error
