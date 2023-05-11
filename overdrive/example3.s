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
        add rbx, 0
        mov rax, 672
        push rax
        lea rax, [rel _label_lambda6510_2ab4]
        mov [rbx + 0], rax
        mov r8, [rsp + 0]
        mov [rbx + 8], r8
        mov rax, rbx
        or rax, 5
        add rbx, 16
        add rsp, 8
        push rax
        lea rax, [rel _ret6511]
        push rax
        mov rax, [rsp + 8]
        push rax
        mov rax, 672
        push rax
        mov rax, [rsp + 8]
        mov r9, rax
        and r9, 7
        cmp r9, 5
        jne _raise_error_align
        xor rax, 5
        mov rax, [rax + 0]
        jmp rax
_ret6511:
        add rsp, 8
        add rsp, 0
        pop rbx
        ret
_label_lambda6510_2ab4:
        mov rax, [rsp + 8]
        xor rax, 5
        mov r9, [rax + 8]
        push r9
        mov rax, [rsp + 8]
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
        add rax, r8
        add rsp, 24
        ret
_raise_error_align:
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        call _raise_error
