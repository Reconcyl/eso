"%macro while_call 0
  movdqu xmm0, [rsp]
  add rsp, 16
  pextrq rax, xmm0, 1
  sub rbp, 16
  movdqu [rbp], xmm0
  call rax
  pop rax
  test rax, rax
  jz %%end
%%start:
  call [rbp]
  call [rbp+8]
  pop rax
  test rax, rax
  jnz %%start
%%end:
  add rbp, 16
%endmacro

%macro cond_call 0
  pop rbx
  pop rax
  test rax, rax
  jz %%end
  call rbx
  %%end:
%endmacro

%define STRING_DATA "34$,,"

"[1[$ø][1+]#$i:[1-$][$øq;1-$$[%1-$$[%1-$$[%1-$$[%%$0>$[%"  lea rax, [rel func_"."]
  push rax
"1]?0=[~33-$$[%1-$$[%1-$$[%1-$$[%1-$$[%1-$$[%1-$$[%3-$$[%1-$$[%1-$$[%1-$$[%1-$$[%1-$$[%$0\>\1-$9>@|$[%10-$$[%1-$$[%2-$$[%1-$$[%1-$$[%1-$$[%12-$$[%16-$$[%2-$$[%1-$$[%1-$$[%$0\>\1-$25>@|$[%27-$$[%1-$$[%1-$$[%69-$$[%%"  ; invalid opcode
"1]?0=[%4q:]?1]?0=[%"  not qword [rsp]
"]?1]?0=[%d;"  push qword "."
"0d:]?1]?0=[%"  pop rax
  or [rsp], rax
"]?1]?0=[8*"  lea rax, [rel vars + "."]
  push rax
"]?1]?0=[%d;$255&256*\65280&256/|"  dw "."
"0d:]?1]?0=[%"  neg qword [rsp]
"]?1]?0=[%1j:"  call getchar
  push rdi
"]?1]?0=[%"  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
"]?1]?0=[%"  lea rax, [rel stack.bottom]
  sub rax, rsp
  sar eax, 3
  push rax
"]?1]?0=[%"  mov rax, [rsp + 16]
  movdqu xmm0, [rsp]
  movdqu [rsp + 8], xmm0
  mov [rsp], rax
"]?1]?0=[%"  cond_call
"]?1]?0=[%"  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
"]?1]?0=[%"  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
"]?1]?0=[%"  pop rax
  push qword [rax]
"]?1]?0=[%"  pop rax
  pop rdx
  mov [rax], rdx
"]?1]?0=[d;10*+d:]?1]?0=[%"  pop rbx
  pop rax
  cqo
  idiv rbx
  push rax
"]?1]?0=[%1n:"  pop rdx
  call putnum
"]?1]?0=[%"  pop rax
  sub [rsp], rax
"]?1]?0=[%1o:"  pop rbx
  call putchar
"]?1]?0=[%"  pop rax
  add [rsp], rax
"]?1]?0=[%"  pop rax
  imul rax, [rsp]
  mov [rsp], rax
"]?1]?0=[%1q:]?1]?0=[%"  pop rax
  and [rsp], rax
"]?1]?0=[%"  pop rax
"]?1]?0=[%"  push qword [rsp]
"]?1]?0=[%"  while_call
"]?1]?0=[%1l:3q:]?1]?0=[%"  pop rax
  call rax
"]?]?1]?0=[%~$184=["  pop rax
  push qword [rsp + 8*rax]
"]?159=["  call flush
"1b:]?0q:]?1]?0=[%~"  lea rdi, [rel string_data + "."]
"2q:]?1]?0=[%~$"  mov eax, "."
"1024>$[%"  call putbigstr
"1]?0=["  call putsmallstr
"]?0q:]?1]?0=[%~"  push qword "."
"0q:]?]#i;[$][\%1-]#%%]c:0[^~$][e;1ø58+$0~>\9>~&$e:~2ø96~=~&&[125~\]?$91~=~&$93~=[%f;1+$f:"func_".":
  sub rbp, 8
  pop qword [rbp]
"c;!"  push qword [rbp]
  add rbp, 8
  ret
"f;]?$39~=[^~]?$34~=[0"%xdefine STRING_DATA STRING_DATA"[^$$34=\0~=|~][",".1+]#"
"%$k;$@+k:~\~]?$123~=[%[^$125=\0~=|~][]#]?]#%"false_main:
"c;!"  jmp finish
"j;["
getchar:
  mov rbx, rsi
  mov r12d, ecx
  mov eax, 0x02000003
  xor edi, edi
  lea rsi, [rel input_buf]
  mov edx, 1
  syscall
  mov rsi, rbx
  mov ecx, r12d
  test eax, eax
  jbe .eof
  movzx edi, byte [rel input_buf]
  ret
  .eof:
  sub rdi, 1
  ret
"]?o;["
putchar:
  cmp ecx, output_buf.len
  jb .sufficient_capacity
  call flush
  .sufficient_capacity:
  mov rsi[rcx], bl
  add ecx, 1
  ret
"1b:]?n;["
putnum:
  mov r12, rdx
  xor ebx, ebx
  mov r13, 0xCCCCCCCCCCCCCCCD
  .count_digits:
  mov rax, rdx
  mov rdi, rdx
  add ebx, 1
  mul r13
  shr rdx, 3
  cmp rdi, 9
  ja .count_digits
  lea rax, [rbx + rcx]
  cmp eax, output_buf.len
  jbe .sufficient_capacity
  call flush
  .sufficient_capacity:
  lea rdi, [rsi + rcx]
  add ecx, ebx
  lea rbx, [rsi + rcx]
  .write_digits:
  mov rax, r12
  sub rbx, 1
  mul r13
  shr rdx, 3
  lea rax, [rdx + rdx*4]
  add rax, rax
  sub r12, rax
  add r12d, 48
  mov [rbx], r12b
  mov r12, rdx
  cmp rbx, rdi
  jne .write_digits
  ret
"1b:]?l;["
putsmallstr:
  lea rbx, [rax + rcx]
  cmp ebx, output_buf.len
  jbe .sufficient_capacity
  mov ebx, eax
  mov r12, rdi
  call flush
  mov rdi, r12

  .sufficient_capacity:
  mov rdx, rsi
  mov rsi, rdi
  lea rdi, rdx[rcx]
  mov ecx, eax
  rep movsb
  mov rsi, rdx
  mov ecx, ebx
  ret

putbigstr:
  test ecx, ecx
  jz .noflush
  call flush
  .noflush:
  mov rbx, rsi
  mov rsi, rdi
  mov edx, eax
  mov eax, 0x02000004
  mov edi, 1
  syscall
  mov rsi, rbx
  ret
"1b:]?b;["
flush:
  mov eax, 0x02000004
  mov edi, 1
  mov edx, ecx
  syscall
  xor ecx, ecx
  ret
"]?"
global start
start:
  mov rax, [rsp + 8]
  mov [rel vars + 0], rax
  mov rbp, rsp
  lea rsp, [rel stack.bottom]
  lea rsi, [rel output_buf]
  xor ecx, ecx
  jmp false_main
finish:
"b;["  call flush
"]?"  mov eax, 0x02000001
  xor rdi, rdi
  syscall
  ud2

string_data:
  db STRING_DATA

section .bss align=4096
  stack:      resq 1024*1024
  .bottom:
  output_buf: resb 4096
  .len:       equ $ - output_buf
  input_buf:  resb 1
  alignb 8
  vars:       resq 26
"