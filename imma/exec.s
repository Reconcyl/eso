.intel_syntax noprefix

.bss

// The main state array is zero-initialized at the start of execution. It
// consists of 2^16 values, each 16 bits, for a total of 2*65KB. However,
// we reserve one extra byte for reasons which will become clear.
.equ MEMSZ, 0x20000
.equ MEMBUFSZ, MEMSZ + 1
MEM: .skip MEMBUFSZ

// Whether to suppress warning messages (indicated by the '-q' flag)
QUIET: .byte 0

.text

.global main
// rdi holds argc, rsi holds argv
main:
    // store the program name in rbp
    mov rbp, [rsi]

    // if one arg is passed, treat it as a filename
    cmp edi, 2
    je .readfile

    // if two args are passed...
    cmp edi, 3
    jne .usage

    // and the first is '-q'...
    lea rbx, [rsi + 8]
    mov rsi, [rbx]
    mov rdi, offset quiet_flag
    call strcmp
    test eax, eax
    jnz .usage

    // then set the quiet flag...
    inc byte ptr [QUIET]
    // and treat the second argument as a filename
    mov rsi, rbx

    // precondition: [rsi + 8] is a pointer to the file name
    .readfile:
    mov rdi, [rsi + 8]
    mov rsi, offset read_mode
    call fopen
    test eax, eax
    jz .ioerror

    // continue reading to MEM in a loop until one of the following:
    // - the buffer is full (rbp == 0)
    // - there isn't anything left to read (fread returns 0)
    // the following state registers are used:
    // - rbx: pointer to the part of the buffer to read from
    // - ebp: remaining size in the buffer
    // - r12: file pointer
    // - r13: errno location - it should be fine to cache it
    //        rather than calling the function every time
    mov rbx, offset MEM
    mov ebp, MEMBUFSZ
    mov r12, rax # from the fopen call
    call __errno_location
    mov r13, rax

    .fillmem:
    // read from the file
    mov rdi, rbx
    mov esi, 1
    mov edx, ebp
    mov rcx, r12
    call fread

    // exit early if errno is nonzero
    mov ebx, [r13]
    test ebx, ebx
    jnz .closeioerror

    // increment buffer and decrement capacity by the number of bytes read
    add rbx, rax
    sub ebp, eax

    // if there is no space left in the buffer, this means we read
    // more than MEMSZ from the file - this should produce a warning
    mov rdi, offset file_too_big
    test ebp, ebp
    jz .done_reading_weird_size

    // otherwise, if this read succeeded, keep reading
    test eax, eax
    jnz .fillmem

    // otherwise, if there was exactly 1 byte of capacity remaining,
    // then the file was exactly the right size
    dec ebp
    jz .done_reading

    // otherwise, produce a warning as the file is smaller than MEMSZ
    mov rdi, offset file_too_small

    // precondition: rdi points to the error message
    .done_reading_weird_size:
    // don't warn if the quiet flag is set
    movzx eax, byte ptr [QUIET]
    test al, al
    jg .done_reading

    // TODO: write to stderr instead (I can't seem to get accessing the
    // global variable to work)
    call puts

    .done_reading:
    mov rdi, r12
    call fclose
    jmp run

    .closeioerror:
    mov rdi, r12
    call fclose

    .ioerror:
    // print the error and return 1
    mov rdi, offset error_prefix
    call perror
    mov eax, 1
    ret

    // print an error message and return 1
    .usage:
    mov rdi, offset usage
    mov rsi, rbp
    mov rdx, rbp
    xor eax, eax
    call printf

    mov eax, 1
    ret

usage:
    .ascii "Usage:\n"
    .ascii "  %s [file]     load and run image file\n"
    .ascii "  %s -q [file]  suppress warnings\n"
    .byte 0

quiet_flag:
    .asciz "-q"

read_mode:
    .asciz "r"

error_prefix:
    .asciz "error"

file_too_big:
    .ascii "warning: The image file was too large. "
    .ascii "All bytes after position 0x20000 have been ignored."
    .byte 0

file_too_small:
    .ascii "warning: The image file was smaller than 0x20000 bytes. "
    .ascii "The remaining memory will be initialized to zero."
    .byte 0

run:
    mov rbp, offset MEM

    .run_ins:

    // load the current IP value and increment it
    movzx ebx, word ptr MEM[0]

    // load the current opcode
    movzx edi, word ptr [rbp + 2*rbx]

    // increment the loaded IP value and store it back
    inc bx
    mov MEM[0], bx

    // if the opcode is out of bounds, move to the next instruction
    cmp edi, 12
    ja .run_ins

    // calculate the address to the handler for this opcode
    .equ JT_SPACING_BITS, 5
    .equ JT_SPACING, 1<<JT_SPACING_BITS
    mov esi, edi
    shl esi, JT_SPACING_BITS
    add rsi, offset .jumptable

    // otherwise...
    // read the first byte from the table entry
    movzx eax, byte ptr [rsi]
    // and increase the IP by that amount
    add MEM[0], ax

    // jump to the next part of the entry
    inc rsi
    jmp rsi

    // preconditions for each entry:
    // - `rbx` holds the index of the first argument to the opcode
    // - `rbp` holds `offset MEM` (for shorter encoding)
    // each entry might clobber `rbx` or anything clobbered by
    // a function (`rax`, `rdi`, etc.)
    .jumptable:

        .org .jumptable + JT_SPACING*0
        .byte 0
        .opcode_halt:
        xor eax, eax
        ret

        .org .jumptable + JT_SPACING*1
        .byte 0
        .opcode_nop:
        jmp .run_ins

        .org .jumptable + JT_SPACING*2
        .byte 1
        .opcode_get:
        movzx eax, word ptr [rbp + 2*rbx]
        movzx eax, word ptr [rbp + 2*rax]
        mov [rbp + 2*rbx], ax
        jmp .run_ins

        .org .jumptable + JT_SPACING*3
        .byte 2
        .opcode_lit:
        movzx eax, word ptr [rbp + 2*rbx]
        inc bx
        movzx ebx, word ptr [rbp + 2*rbx]
        mov [rbp + 2*rbx], ax
        jmp .run_ins

        .org .jumptable + JT_SPACING*4
        .byte 1
        .opcode_not:
        xor eax, eax
        cmp word ptr [rbp + 2*rbx], 0
        sete al
        mov [rbp + 2*rbx], ax
        jmp .run_ins

        .org .jumptable + JT_SPACING*5
        .byte 2
        .opcode_add:
        // use rdi to store a pointer to the first argument,
        // because we're going to be changing `bx`
        lea rdi, [rbp + 2*rbx]
        inc bx
        movzx eax, word ptr [rbp + 2*rbx]
        add [rdi], ax
        jmp .run_ins

        .org .jumptable + JT_SPACING*6
        .byte 2
        .opcode_mul:
        // imul doesn't permit an indirect dst, only indirect source,
        // so this works a bit differently from addition
        lea rdi, [rbp + 2*rbx]
        movzx eax, word ptr [rdi]
        inc bx
        imul ax, [rbp + 2*rbx]
        mov [rdi], ax
        jmp .run_ins

        .org .jumptable + JT_SPACING*7
        .byte 2
        .opcode_max:
        lea rdi, [rbp + 2*rbx]
        movzx eax, word ptr [rdi]
        inc bx
        movzx ebx, word ptr [rbp + 2*rbx]
        cmp eax, ebx
        cmova eax, ebx
        mov [rdi], ax
        jmp .run_ins

        .org .jumptable + JT_SPACING*8
        .byte 3
        .opcode_dmp:
        mov rdi, offset .not_implemented_msg
        call puts
        xor eax, eax
        ret

        .org .jumptable + JT_SPACING*9
        .byte 3
        .opcode_sav:
        jmp .opcode_dmp
        .not_implemented_msg:
        .asciz "not implemented"

        .org .jumptable + JT_SPACING*10
        .byte 1
        .opcode_chr:
        movzx edi, byte ptr [rbp + 2*rbx]
        call putchar
        jmp .run_ins

        .org .jumptable + JT_SPACING*11
        .byte 1
        .opcode_num:
        mov rdi, offset .fmt_string
        movzx esi, word ptr [rbp + 2*rbx]
        xor eax, eax
        call printf
        jmp .run_ins
        .fmt_string: .asciz "%d"

        .org .jumptable + JT_SPACING*12
        .byte 1
        .opcode_chi:
        call getchar
        mov edi, ebx
        mov ebx, 0xffff # bx == -1
        test eax, eax
        // it's not guaranteed that EOF == -1, only that EOF < 0
        cmovns ebx, eax
        mov [rbp + 2*rdi], bx
        jmp .run_ins
