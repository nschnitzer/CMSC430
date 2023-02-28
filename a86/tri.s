        default rel
        section .text
        global entry
entry:
        mov rbx, 35             ; the "input"
;;; tri: a recursive function for computing nth
;;; triangular number, where n is given in rbx.
tri:
        cmp rbx, 0              ; if rbx = 0, done
        je done
        push rbx                ; save rbx
        sub rbx, 1
        call tri                ; compute tri(rbx-1) in rax
        pop rbx                 ; restore rbx
        add rax, rbx            ; result is rbx+tri(rbx-1)
        ret
done:                           ; jump here for base case
        mov rax, 0              ; return 0
        ret
