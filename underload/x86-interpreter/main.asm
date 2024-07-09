bits 64

section .text

; start of region counted for golfing
text_start:

; Syscall calling conventions
; ===========================
; 1. User-level applications use as integer registers for passing the sequence %rdi, %rsi, %rdx, %rcx, %r8 and %r9.
;    The kernel interface uses %rdi, %rsi, %rdx, %r10, %r8 and %r9.
; 2. A system-call is done via the syscall instruction. The kernel destroys registers %rcx and %r11.
; 3. The number of the syscall has to be passed in register %rax.
; 4. System-calls are limited to six arguments, no argument is passed directly on the stack.
; 5. Returning from the syscall, register %rax contains the result of the system-call.
;    A value in the range between -4095 and -1 indicates an error, it is -errno.
; 6. Only values of class INTEGER or class MEMORY are passed to the kernel.

global _start
_start:
        mov r8, rsp                     ; track the top of the stack
        lea rbx, [rsp - 2*1024*1024]    ; use memory further down in the OS stack to store the data stack
        ; xor ebp, ebp                  ; ebp stores the nesting depth

exec_loop:
        ; is there a character on the control stack?
        cmp rsp, r8
        jb .run_char

        ; if not, read a character
        xor eax, eax    ; READ
        xor edi, edi    ; STDIN_FILENO
        dec rsp
        mov rsi, rsp    ; buf = rsp
        push 1
        pop rdx         ; len = 1
        syscall
        test rax, rax
        jle stop

    .run_char:
        mov al, byte [rsp]
        inc rsp

        cmp al, '('
        je open_paren
        cmp al, ')'
        je close_paren

        test ebp, ebp
        jnz append_char_to_stack

        cmp al, '~'
        je cmd_tilde
        cmp al, ':'
        je cmd_colon
        cmp al, '!'
        je cmd_exclamation
        cmp al, '*'
        je cmd_star
        cmp al, 'a'
        je cmd_a
        cmp al, '^'
        je cmd_caret
        cmp al, 'S'
        je cmd_print

        jmp exec_loop

close_paren:
        dec ebp
        jz exec_loop
        jmp append_char_to_stack

open_paren:
        test ebp, ebp
        cmovz eax, ebp  ; if ebp == 0 (active): set al = 0
        inc ebp

append_char_to_stack:
        dec rbx
        mov byte [rbx], al
.trampoline:
        jmp exec_loop

cmd_tilde:
        mov rax, "!a*^):*^"
        push rax
        mov rax, "(a*a*:*^"
        push rax
        mov rax, "a(!a)(!)"
        push rax
        jmp append_char_to_stack.trampoline

cmd_colon:
        call measure_tos
        mov rcx, rbx

    .loop:
        dec rsi
        mov al, byte [rsi]
        dec rbx
        mov byte [rbx], al
        cmp rsi, rcx
        jne .loop

        jmp append_char_to_stack.trampoline

cmd_exclamation:
        inc rbx
        cmp byte [rbx - 1], 0
        jnz cmd_exclamation
        jmp append_char_to_stack.trampoline

cmd_star:
        mov al, byte [rbx]
        inc rbx
        push rbx

    .loop:
        test al, al
        jz common_operand_end
        xchg al, byte [rbx]
        inc rbx
        jmp .loop

cmd_a:
        push rbx

    .loop:
        mov al, byte [rbx]
        mov byte [rbx - 1], al
        inc rbx
        test al, al
        jnz .loop

        mov byte [rbx - 2], '('
        pop rbx
        sub rbx, 2
        mov byte [rbx], ')'

    .trampoline:
        jmp append_char_to_stack.trampoline

cmd_caret:
        mov al, byte [rbx]
        test al, al
        jz cmd_exclamation  ; will handle removing the final () from the stack
        dec rsp
        mov byte [rsp], al
        inc rbx
        jmp cmd_caret

common_operand_end:
        pop rbx
        jmp cmd_a.trampoline

cmd_print:
        call measure_tos
        push rsi

        xor edi, edi
        inc edi             ; edi = 1 = STDOUT_FILENO
        mov edx, edi        ; len = 1

    .loop:
        cmp rsi, rbx
        je common_operand_end
        dec rsi
        mov eax, edi        ; eax = 1 = WRITE
        syscall
        jmp .loop

measure_tos:
        mov rsi, rbx

    .loop:
        inc rsi
        cmp byte [rsi - 1], 0
        jnz .loop

        ret

stop:
        push 60
        pop rax
        xor edi, edi
        syscall

text_end:
; end of region counted for golfing
        ud2

%assign SZ text_end - text_start
%warning Text size: SZ bytes
