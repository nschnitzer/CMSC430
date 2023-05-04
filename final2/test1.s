        default rel
        section .text
        extern _peek_byte
        extern _read_byte
        extern _write_byte
        extern _raise_error
        global _entry
_entry:
        push r14
        mov r14, 0
        push r12
        mov r12, rsi
        push rbx
        mov rbx, rdi
        add rbx, 0
        lea rax, [rel _label_lambdaPredicate6551_2cbc]
        mov [rbx + 0], rax
        mov rax, rbx
        or rax, 5
        add rbx, 8
        lea rax, [rel _label_lambda6553_2cbe]
        mov [rbx + 0], rax
        mov rax, rbx
        or rax, 5
        add rbx, 8
        lea rax, [rel _label_lambdaPredicate6551_2cbc]
        mov [r12 + 0], rax
        lea rax, [rel _label_lambda6553_2cbe]
        mov [r12 + 8], rax
        add r14, 1
        add r12, 16
        push r15
        mov r15, 0
        mov rax, 1
        mov [rbx + 0], rax
        mov rax, 120
        mov [rbx + 8], eax
        mov rax, rbx
        or rax, 4
        add rbx, 16
        push rcx
        mov rcx, rax
_raise6554:
        cmp r15, r14
        je _raise6555
        lea rax, [rel _ret6557]
        push rax
        push rcx
        mov rax, [r12 + 8]
        jmp rax
_ret6557:
        cmp rax, 24
        jne _raise6556
        cmp rax, 56
        jne _raise6555
        add r15, 1
        sub r12, 16
        jmp _raise6554
_raise6555:
        pop rcx
        pop r15
        jmp _raise_error_align
_raise6556:
        lea rax, [rel _ret6558]
        push rax
        push rcx
        mov rax, [r12 + 0]
        jmp rax
_ret6558:
        pop rcx
        pop r15
        ret
        mov rcx, 1
        sub r12, 8
        sub r14, rcx
        add rsp, 0
        pop rbx
        pop r12
        pop r14
        ret
_label_lambdaPredicate6551_2cbc:
        mov rax, [rsp + 8]
        xor rax, 5
        mov rax, [rsp + 0]
        and rax, 7
        cmp rax, 4
        mov rax, 24
        je _g6559
        mov rax, 56
_g6559:
        add rsp, 16
        ret
_label_lambda6553_2cbe:
        mov rax, [rsp + 8]
        xor rax, 5
        mov rax, 3
        mov [rbx + 0], rax
        mov rax, 103
        mov [rbx + 8], eax
        mov rax, 111
        mov [rbx + 12], eax
        mov rax, 116
        mov [rbx + 16], eax
        mov rax, rbx
        or rax, 4
        add rbx, 24
        push rax
        mov rax, [rsp + 8]
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        add rsp, 16
        ret
_raise_error_align:
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        call _raise_error
