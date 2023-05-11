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
        mov rax, 16
        push rax
        lea rax, [rel _ret6512]
        push rax
        mov rax, [rsp + 8]
        mov r9, rax
        and r9, 15
        cmp r9, 0
        jne _raise_error_align
        cmp rax, 0
        mov rax, 56
        mov r9, 24
        cmove rax, r9
        cmp rax, 56
        je _if6513
        lea rax, [rel _label_lambda6510_2ab4]
        mov [rbx + 0], rax
        mov rax, rbx
        or rax, 5
        add rbx, 8
        jmp _if6514
_if6513:
        lea rax, [rel _label_lambda6511_2ab5]
        mov [rbx + 0], rax
        mov rax, rbx
        or rax, 5
        add rbx, 8
_if6514:
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
_ret6512:
        add rsp, 8
        add rsp, 0
        pop rbx
        ret
_label_lambda6510_2ab4:
        mov rax, [rsp + 8]
        xor rax, 5
        mov rax, [rsp + 0]
        add rsp, 16
        ret
_label_lambda6511_2ab5:
        mov rax, [rsp + 8]
        xor rax, 5
        mov rax, [rsp + 0]
        push rax
        mov rax, 672
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
        add rsp, 16
        ret
_raise_error_align:
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        call _raise_error
