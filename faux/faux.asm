%macro while_call 0
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

%define STRING_DATA ""

%xdefine STRING_DATA STRING_DATA,37,109,97,99,114,111,32,119,104,105,108,101,95,99,97,108,108,32,48,10,32,32,109,111,118,100,113,117,32,120,109,109,48,44,32,91,114,115,112,93,10,32,32,97,100,100,32,114,115,112,44,32,49,54,10,32,32,112,101,120,116,114,113,32,114,97,120,44,32,120,109,109,48,44,32,49,10,32,32,115,117,98,32,114,98,112,44,32,49,54,10,32,32,109,111,118,100,113,117,32,91,114,98,112,93,44,32,120,109,109,48,10,32,32,99,97,108,108,32,114,97,120,10,32,32,112,111,112,32,114,97,120,10,32,32,116,101,115,116,32,114,97,120,44,32,114,97,120,10,32,32,106,122,32,37,37,101,110,100,10,37,37,115,116,97,114,116,58,10,32,32,99,97,108,108,32,91,114,98,112,93,10,32,32,99,97,108,108,32,91,114,98,112,43,56,93,10,32,32,112,111,112,32,114,97,120,10,32,32,116,101,115,116,32,114,97,120,44,32,114,97,120,10,32,32,106,110,122,32,37,37,115,116,97,114,116,10,37,37,101,110,100,58,10,32,32,97,100,100,32,114,98,112,44,32,49,54,10,37,101,110,100,109,97,99,114,111,10,10,37,109,97,99,114,111,32,99,111,110,100,95,99,97,108,108,32,48,10,32,32,112,111,112,32,114,98,120,10,32,32,112,111,112,32,114,97,120,10,32,32,116,101,115,116,32,114,97,120,44,32,114,97,120,10,32,32,106,122,32,37,37,101,110,100,10,32,32,99,97,108,108,32,114,98,120,10,32,32,37,37,101,110,100,58,10,37,101,110,100,109,97,99,114,111,10,10,37,100,101,102,105,110,101,32,83,84,82,73,78,71,95,68,65,84,65,32
%xdefine STRING_DATA STRING_DATA,10,10
func_1:
  sub rbp, 8
  pop qword [rbp]
  push qword [rsp]
  pop rax
  push qword [rsp + 8*rax]
  push qword [rbp]
  add rbp, 8
  ret
func_2:
  sub rbp, 8
  pop qword [rbp]
  push qword 1
  pop rax
  add [rsp], rax
  push qword [rbp]
  add rbp, 8
  ret
func_3:
  sub rbp, 8
  pop qword [rbp]
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,108,101,97,32,114,97,120,44,32,91,114,101,108,32,102,117,110,99,95
%xdefine STRING_DATA STRING_DATA,93,10,32,32,112,117,115,104,32,114,97,120,10
func_4:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 388]
  mov eax, 21
  call putsmallstr
  pop rdx
  call putnum
  lea rdi, [rel string_data + 409]
  mov eax, 13
  call putsmallstr
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,59,32,105,110,118,97,108,105,100,32,111,112,99,111,100,101,10
func_5:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  pop rax
  lea rdi, [rel string_data + 422]
  mov eax, 19
  call putsmallstr
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
func_6:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 4
  lea rax, [rel vars + 128]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_7:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 69
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_5]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_6]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,110,111,116,32,113,119,111,114,100,32,91,114,115,112,93,10
func_8:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 441]
  mov eax, 18
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_9:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_7]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_8]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,117,115,104,32,113,119,111,114,100,32
%xdefine STRING_DATA STRING_DATA,10
func_10:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rax, [rel vars + 24]
  push rax
  pop rax
  push qword [rax]
  lea rdi, [rel string_data + 459]
  mov eax, 13
  call putsmallstr
  pop rdx
  call putnum
  lea rdi, [rel string_data + 472]
  mov eax, 1
  call putsmallstr
  push qword 0
  lea rax, [rel vars + 24]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_11:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_9]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_10]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,111,114,32,91,114,115,112,93,44,32,114,97,120,10
func_12:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 473]
  mov eax, 26
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_13:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 27
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_11]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_12]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,108,101,97,32,114,97,120,44,32,91,114,101,108,32,118,97,114,115,32,43,32
%xdefine STRING_DATA STRING_DATA,93,10,32,32,112,117,115,104,32,114,97,120,10
func_14:
  sub rbp, 8
  pop qword [rbp]
  push qword 8
  pop rax
  imul rax, [rsp]
  mov [rsp], rax
  lea rdi, [rel string_data + 499]
  mov eax, 23
  call putsmallstr
  pop rdx
  call putnum
  lea rdi, [rel string_data + 522]
  mov eax, 13
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_15:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword [rsp]
  push qword 0
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword 25
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
  mov rax, [rsp + 16]
  movdqu xmm0, [rsp]
  movdqu [rsp + 8], xmm0
  mov [rsp], rax
  pop rax
  or [rsp], rax
  push qword [rsp]
  lea rax, [rel func_13]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_14]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,100,119,32
%xdefine STRING_DATA STRING_DATA,10
func_16:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rax, [rel vars + 24]
  push rax
  pop rax
  push qword [rax]
  push qword [rsp]
  push qword 255
  pop rax
  and [rsp], rax
  push qword 256
  pop rax
  imul rax, [rsp]
  mov [rsp], rax
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  push qword 65280
  pop rax
  and [rsp], rax
  push qword 256
  pop rbx
  pop rax
  cqo
  idiv rbx
  push rax
  pop rax
  or [rsp], rax
  lea rdi, [rel string_data + 535]
  mov eax, 5
  call putsmallstr
  pop rdx
  call putnum
  lea rdi, [rel string_data + 540]
  mov eax, 1
  call putsmallstr
  push qword 0
  lea rax, [rel vars + 24]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_17:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_15]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_16]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,110,101,103,32,113,119,111,114,100,32,91,114,115,112,93,10
func_18:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 541]
  mov eax, 18
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_19:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_17]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_18]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,99,97,108,108,32,103,101,116,99,104,97,114,10,32,32,112,117,115,104,32,114,100,105,10
func_20:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  lea rax, [rel vars + 72]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  lea rdi, [rel string_data + 559]
  mov eax, 26
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_21:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 2
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_19]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_20]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,109,111,118,100,113,117,32,120,109,109,48,44,32,91,114,115,112,93,10,32,32,115,104,117,102,112,100,32,120,109,109,48,44,32,120,109,109,48,44,32,49,10,32,32,109,111,118,100,113,117,32,91,114,115,112,93,44,32,120,109,109,48,10
func_22:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 585]
  mov eax, 65
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_23:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 16
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_21]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_22]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,108,101,97,32,114,97,120,44,32,91,114,101,108,32,115,116,97,99,107,46,98,111,116,116,111,109,93,10,32,32,115,117,98,32,114,97,120,44,32,114,115,112,10,32,32,115,97,114,32,101,97,120,44,32,51,10,32,32,112,117,115,104,32,114,97,120,10
func_24:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 650]
  mov eax, 69
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_25:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 12
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_23]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_24]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,109,111,118,32,114,97,120,44,32,91,114,115,112,32,43,32,49,54,93,10,32,32,109,111,118,100,113,117,32,120,109,109,48,44,32,91,114,115,112,93,10,32,32,109,111,118,100,113,117,32,91,114,115,112,32,43,32,56,93,44,32,120,109,109,48,10,32,32,109,111,118,32,91,114,115,112,93,44,32,114,97,120,10
func_26:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 719]
  mov eax, 85
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_27:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_25]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_26]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,99,111,110,100,95,99,97,108,108,10
func_28:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 804]
  mov eax, 12
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_29:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_27]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_28]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,120,111,114,32,101,98,120,44,32,101,98,120,10,32,32,99,109,112,32,114,97,120,44,32,91,114,115,112,93,10,32,32,115,101,116,108,32,98,108,10,32,32,110,101,103,32,114,98,120,10,32,32,109,111,118,32,91,114,115,112,93,44,32,114,98,120,10
func_30:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 816]
  mov eax, 79
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_31:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_29]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_30]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,120,111,114,32,101,98,120,44,32,101,98,120,10,32,32,99,109,112,32,114,97,120,44,32,91,114,115,112,93,10,32,32,115,101,116,101,32,98,108,10,32,32,110,101,103,32,114,98,120,10,32,32,109,111,118,32,91,114,115,112,93,44,32,114,98,120,10
func_32:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 895]
  mov eax, 79
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_33:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 2
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_31]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_32]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,112,117,115,104,32,113,119,111,114,100,32,91,114,97,120,93,10
func_34:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 974]
  mov eax, 29
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_35:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_33]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_34]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,112,111,112,32,114,100,120,10,32,32,109,111,118,32,91,114,97,120,93,44,32,114,100,120,10
func_36:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1003]
  mov eax, 37
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_37:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 10
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_35]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_36]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
func_38:
  sub rbp, 8
  pop qword [rbp]
  lea rax, [rel vars + 24]
  push rax
  pop rax
  push qword [rax]
  push qword 10
  pop rax
  imul rax, [rsp]
  mov [rsp], rax
  pop rax
  add [rsp], rax
  lea rax, [rel vars + 24]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_39:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword [rsp]
  push qword 0
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword 9
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
  mov rax, [rsp + 16]
  movdqu xmm0, [rsp]
  movdqu [rsp + 8], xmm0
  mov [rsp], rax
  pop rax
  or [rsp], rax
  push qword [rsp]
  lea rax, [rel func_37]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_38]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,98,120,10,32,32,112,111,112,32,114,97,120,10,32,32,99,113,111,10,32,32,105,100,105,118,32,114,98,120,10,32,32,112,117,115,104,32,114,97,120,10
func_40:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1040]
  mov eax, 48
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_41:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_39]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_40]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,100,120,10,32,32,99,97,108,108,32,112,117,116,110,117,109,10
func_42:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  lea rax, [rel vars + 104]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  lea rdi, [rel string_data + 1088]
  mov eax, 24
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_43:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_41]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_42]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,115,117,98,32,91,114,115,112,93,44,32,114,97,120,10
func_44:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1112]
  mov eax, 27
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_45:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_43]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_44]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,98,120,10,32,32,99,97,108,108,32,112,117,116,99,104,97,114,10
func_46:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  lea rax, [rel vars + 112]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  lea rdi, [rel string_data + 1139]
  mov eax, 25
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_47:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_45]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_46]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,97,100,100,32,91,114,115,112,93,44,32,114,97,120,10
func_48:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1164]
  mov eax, 27
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_49:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_47]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_48]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,105,109,117,108,32,114,97,120,44,32,91,114,115,112,93,10,32,32,109,111,118,32,91,114,115,112,93,44,32,114,97,120,10
func_50:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1191]
  mov eax, 45
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_51:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 3
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_49]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_50]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
func_52:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  lea rax, [rel vars + 128]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_53:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_51]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_52]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,97,110,100,32,91,114,115,112,93,44,32,114,97,120,10
func_54:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1236]
  mov eax, 27
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_55:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_53]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_54]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10
func_56:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1263]
  mov eax, 10
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_57:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_55]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_56]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,117,115,104,32,113,119,111,114,100,32,91,114,115,112,93,10
func_58:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1273]
  mov eax, 19
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_59:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_57]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_58]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,119,104,105,108,101,95,99,97,108,108,10
func_60:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1292]
  mov eax, 13
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_61:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_59]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_60]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
func_62:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  lea rax, [rel vars + 88]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword 3
  lea rax, [rel vars + 128]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_63:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_61]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_62]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,99,97,108,108,32,114,97,120,10
func_64:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1305]
  mov eax, 21
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_65:
  sub rbp, 8
  pop qword [rbp]
  not qword [rsp]
  push qword 33
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_63]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_64]
  push rax
  cond_call
  push qword [rbp]
  add rbp, 8
  ret
func_66:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  pop rax
  push qword [rsp]
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
  push qword [rsp]
  lea rax, [rel func_4]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_65]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,111,112,32,114,97,120,10,32,32,112,117,115,104,32,113,119,111,114,100,32,91,114,115,112,32,43,32,56,42,114,97,120,93,10
func_67:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 1326]
  mov eax, 37
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,99,97,108,108,32,102,108,117,115,104,10
func_68:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 1363]
  mov eax, 13
  call putsmallstr
  push qword 1
  lea rax, [rel vars + 8]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_69:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  not qword [rsp]
  push qword [rsp]
  push qword 184
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_67]
  push rax
  cond_call
  push qword 159
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_68]
  push rax
  cond_call
  push qword 0
  lea rax, [rel vars + 128]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_70:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_66]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_69]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,108,101,97,32,114,100,105,44,32,91,114,101,108,32,115,116,114,105,110,103,95,100,97,116,97,32,43,32
%xdefine STRING_DATA STRING_DATA,93,10
func_71:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  not qword [rsp]
  lea rdi, [rel string_data + 1376]
  mov eax, 30
  call putsmallstr
  pop rdx
  call putnum
  lea rdi, [rel string_data + 1406]
  mov eax, 2
  call putsmallstr
  push qword 2
  lea rax, [rel vars + 128]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_72:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_70]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_71]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,109,111,118,32,101,97,120,44,32
%xdefine STRING_DATA STRING_DATA,10
%xdefine STRING_DATA STRING_DATA,32,32,99,97,108,108,32,112,117,116,98,105,103,115,116,114,10
func_73:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rdi, [rel string_data + 1420]
  mov eax, 17
  call putsmallstr
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,99,97,108,108,32,112,117,116,115,109,97,108,108,115,116,114,10
func_74:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 1437]
  mov eax, 19
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
func_75:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  not qword [rsp]
  push qword [rsp]
  lea rdi, [rel string_data + 1408]
  mov eax, 11
  call putsmallstr
  pop rdx
  call putnum
  lea rdi, [rel string_data + 1419]
  mov eax, 1
  call putsmallstr
  push qword 1024
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
  push qword [rsp]
  lea rax, [rel func_73]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_74]
  push rax
  cond_call
  push qword 0
  lea rax, [rel vars + 128]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_76:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_72]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_75]
  push rax
  cond_call
  push qword 1
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,112,117,115,104,32,113,119,111,114,100,32
%xdefine STRING_DATA STRING_DATA,10
func_77:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  not qword [rsp]
  lea rdi, [rel string_data + 1456]
  mov eax, 13
  call putsmallstr
  pop rdx
  call putnum
  lea rdi, [rel string_data + 1469]
  mov eax, 1
  call putsmallstr
  push qword 0
  lea rax, [rel vars + 128]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
func_78:
  sub rbp, 8
  pop qword [rbp]
  push qword [rsp]
  pop rax
  push qword [rsp + 8*rax]
  lea rax, [rel vars + 128]
  push rax
  pop rax
  push qword [rax]
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rsp]
  push qword [rsp]
  lea rax, [rel func_76]
  push rax
  cond_call
  push qword 0
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_77]
  push rax
  cond_call
  push qword [rbp]
  add rbp, 8
  ret
func_79:
  sub rbp, 8
  pop qword [rbp]
  push qword [rsp]
  push qword [rbp]
  add rbp, 8
  ret
func_80:
  sub rbp, 8
  pop qword [rbp]
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  pop rax
  push qword 1
  pop rax
  sub [rsp], rax
  push qword [rbp]
  add rbp, 8
  ret
func_81:
  sub rbp, 8
  pop qword [rbp]
  push qword 1
  lea rax, [rel func_1]
  push rax
  lea rax, [rel func_2]
  push rax
  while_call
  push qword [rsp]
  lea rax, [rel vars + 64]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  lea rax, [rel func_3]
  push rax
  lea rax, [rel func_78]
  push rax
  while_call
  lea rax, [rel vars + 64]
  push rax
  pop rax
  push qword [rax]
  lea rax, [rel func_79]
  push rax
  lea rax, [rel func_80]
  push rax
  while_call
  pop rax
  pop rax
  push qword [rbp]
  add rbp, 8
  ret
func_82:
  sub rbp, 8
  pop qword [rbp]
  call getchar
  push rdi
  not qword [rsp]
  push qword [rsp]
  push qword [rbp]
  add rbp, 8
  ret
func_83:
  sub rbp, 8
  pop qword [rbp]
  push qword 125
  not qword [rsp]
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,102,117,110,99,95
%xdefine STRING_DATA STRING_DATA,58,10,32,32,115,117,98,32,114,98,112,44,32,56,10,32,32,112,111,112,32,113,119,111,114,100,32,91,114,98,112,93,10
%xdefine STRING_DATA STRING_DATA,32,32,112,117,115,104,32,113,119,111,114,100,32,91,114,98,112,93,10,32,32,97,100,100,32,114,98,112,44,32,56,10,32,32,114,101,116,10
func_84:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rax, [rel vars + 40]
  push rax
  pop rax
  push qword [rax]
  push qword 1
  pop rax
  add [rsp], rax
  push qword [rsp]
  lea rax, [rel vars + 40]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  lea rdi, [rel string_data + 1470]
  mov eax, 5
  call putsmallstr
  pop rdx
  call putnum
  lea rdi, [rel string_data + 1475]
  mov eax, 33
  call putsmallstr
  lea rax, [rel vars + 16]
  push rax
  pop rax
  push qword [rax]
  pop rax
  call rax
  lea rdi, [rel string_data + 1508]
  mov eax, 38
  call putsmallstr
  lea rax, [rel vars + 40]
  push rax
  pop rax
  push qword [rax]
  push qword [rbp]
  add rbp, 8
  ret
func_85:
  sub rbp, 8
  pop qword [rbp]
  call getchar
  push rdi
  not qword [rsp]
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,37,120,100,101,102,105,110,101,32,83,84,82,73,78,71,95,68,65,84,65,32,83,84,82,73,78,71,95,68,65,84,65
func_86:
  sub rbp, 8
  pop qword [rbp]
  call getchar
  push rdi
  push qword [rsp]
  push qword [rsp]
  push qword 34
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  push qword 0
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  pop rax
  or [rsp], rax
  not qword [rsp]
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,44
func_87:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 1578]
  mov eax, 1
  call putsmallstr
  pop rdx
  call putnum
  push qword 1
  pop rax
  add [rsp], rax
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,10
func_88:
  sub rbp, 8
  pop qword [rbp]
  push qword 0
  lea rdi, [rel string_data + 1546]
  mov eax, 32
  call putsmallstr
  lea rax, [rel func_86]
  push rax
  lea rax, [rel func_87]
  push rax
  while_call
  lea rdi, [rel string_data + 1579]
  mov eax, 1
  call putsmallstr
  pop rax
  push qword [rsp]
  lea rax, [rel vars + 80]
  push rax
  pop rax
  push qword [rax]
  push qword [rsp]
  mov rax, [rsp + 16]
  movdqu xmm0, [rsp]
  movdqu [rsp + 8], xmm0
  mov [rsp], rax
  pop rax
  add [rsp], rax
  lea rax, [rel vars + 80]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  not qword [rsp]
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  not qword [rsp]
  push qword [rbp]
  add rbp, 8
  ret
func_89:
  sub rbp, 8
  pop qword [rbp]
  call getchar
  push rdi
  push qword [rsp]
  push qword 125
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  push qword 0
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  pop rax
  or [rsp], rax
  not qword [rsp]
  push qword [rbp]
  add rbp, 8
  ret
func_90:
  sub rbp, 8
  pop qword [rbp]
  push qword [rbp]
  add rbp, 8
  ret
func_91:
  sub rbp, 8
  pop qword [rbp]
  pop rax
  lea rax, [rel func_89]
  push rax
  lea rax, [rel func_90]
  push rax
  while_call
  push qword [rbp]
  add rbp, 8
  ret
func_92:
  sub rbp, 8
  pop qword [rbp]
  lea rax, [rel vars + 32]
  push rax
  pop rax
  push qword [rax]
  push qword 1
  pop rax
  push qword [rsp + 8*rax]
  push qword 58
  pop rax
  add [rsp], rax
  push qword [rsp]
  push qword 0
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
  movdqu xmm0, [rsp]
  shufpd xmm0, xmm0, 1
  movdqu [rsp], xmm0
  push qword 9
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  setl bl
  neg rbx
  mov [rsp], rbx
  not qword [rsp]
  pop rax
  and [rsp], rax
  push qword [rsp]
  lea rax, [rel vars + 32]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  not qword [rsp]
  push qword 2
  pop rax
  push qword [rsp + 8*rax]
  push qword 96
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  not qword [rsp]
  pop rax
  and [rsp], rax
  pop rax
  and [rsp], rax
  lea rax, [rel func_83]
  push rax
  cond_call
  push qword [rsp]
  push qword 91
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  not qword [rsp]
  pop rax
  and [rsp], rax
  push qword [rsp]
  push qword 93
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_84]
  push rax
  cond_call
  push qword [rsp]
  push qword 39
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_85]
  push rax
  cond_call
  push qword [rsp]
  push qword 34
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_88]
  push rax
  cond_call
  push qword [rsp]
  push qword 123
  not qword [rsp]
  pop rax
  xor ebx, ebx
  cmp rax, [rsp]
  sete bl
  neg rbx
  mov [rsp], rbx
  lea rax, [rel func_91]
  push rax
  cond_call
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,102,97,108,115,101,95,109,97,105,110,58,10
%xdefine STRING_DATA STRING_DATA,32,32,106,109,112,32,102,105,110,105,115,104,10
%xdefine STRING_DATA STRING_DATA,10,103,101,116,99,104,97,114,58,10,32,32,109,111,118,32,114,98,120,44,32,114,115,105,10,32,32,109,111,118,32,114,49,50,100,44,32,101,99,120,10,32,32,109,111,118,32,101,97,120,44,32,48,120,48,50,48,48,48,48,48,51,10,32,32,120,111,114,32,101,100,105,44,32,101,100,105,10,32,32,108,101,97,32,114,115,105,44,32,91,114,101,108,32,105,110,112,117,116,95,98,117,102,93,10,32,32,109,111,118,32,101,100,120,44,32,49,10,32,32,115,121,115,99,97,108,108,10,32,32,109,111,118,32,114,115,105,44,32,114,98,120,10,32,32,109,111,118,32,101,99,120,44,32,114,49,50,100,10,32,32,116,101,115,116,32,101,97,120,44,32,101,97,120,10,32,32,106,98,101,32,46,101,111,102,10,32,32,109,111,118,122,120,32,101,100,105,44,32,98,121,116,101,32,91,114,101,108,32,105,110,112,117,116,95,98,117,102,93,10,32,32,114,101,116,10,32,32,46,101,111,102,58,10,32,32,115,117,98,32,114,100,105,44,32,49,10,32,32,114,101,116,10
func_93:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 1605]
  mov eax, 253
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,10,112,117,116,99,104,97,114,58,10,32,32,99,109,112,32,101,99,120,44,32,111,117,116,112,117,116,95,98,117,102,46,108,101,110,10,32,32,106,98,32,46,115,117,102,102,105,99,105,101,110,116,95,99,97,112,97,99,105,116,121,10,32,32,99,97,108,108,32,102,108,117,115,104,10,32,32,46,115,117,102,102,105,99,105,101,110,116,95,99,97,112,97,99,105,116,121,58,10,32,32,109,111,118,32,114,115,105,91,114,99,120,93,44,32,98,108,10,32,32,97,100,100,32,101,99,120,44,32,49,10,32,32,114,101,116,10
func_94:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 1858]
  mov eax, 137
  call putsmallstr
  push qword 1
  lea rax, [rel vars + 8]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,10,112,117,116,110,117,109,58,10,32,32,109,111,118,32,114,49,50,44,32,114,100,120,10,32,32,120,111,114,32,101,98,120,44,32,101,98,120,10,32,32,109,111,118,32,114,49,51,44,32,48,120,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,68,10,32,32,46,99,111,117,110,116,95,100,105,103,105,116,115,58,10,32,32,109,111,118,32,114,97,120,44,32,114,100,120,10,32,32,109,111,118,32,114,100,105,44,32,114,100,120,10,32,32,97,100,100,32,101,98,120,44,32,49,10,32,32,109,117,108,32,114,49,51,10,32,32,115,104,114,32,114,100,120,44,32,51,10,32,32,99,109,112,32,114,100,105,44,32,57,10,32,32,106,97,32,46,99,111,117,110,116,95,100,105,103,105,116,115,10,32,32,108,101,97,32,114,97,120,44,32,91,114,98,120,32,43,32,114,99,120,93,10,32,32,99,109,112,32,101,97,120,44,32,111,117,116,112,117,116,95,98,117,102,46,108,101,110,10,32,32,106,98,101,32,46,115,117,102,102,105,99,105,101,110,116,95,99,97,112,97,99,105,116,121,10,32,32,99,97,108,108,32,102,108,117,115,104,10,32,32,46,115,117,102,102,105,99,105,101,110,116,95,99,97,112,97,99,105,116,121,58,10,32,32,108,101,97,32,114,100,105,44,32,91,114,115,105,32,43,32,114,99,120,93,10,32,32,97,100,100,32,101,99,120,44,32,101,98,120,10,32,32,108,101,97,32,114,98,120,44,32,91,114,115,105,32,43,32,114,99,120,93,10,32,32,46,119,114,105,116,101,95,100,105,103,105,116,115,58,10,32,32,109,111,118,32,114,97,120,44,32,114,49,50,10,32,32,115,117,98,32,114,98,120,44,32,49,10,32,32,109,117,108,32,114,49,51,10,32,32,115,104,114,32,114,100,120,44,32,51,10,32,32,108,101,97,32,114,97,120,44,32,91,114,100,120,32,43,32,114,100,120,42,52,93,10,32,32,97,100,100,32,114,97,120,44,32,114,97,120,10,32,32,115,117,98,32,114,49,50,44,32,114,97,120,10,32,32,97,100,100,32,114,49,50,100,44,32,52,56,10,32,32,109,111,118,32,91,114,98,120,93,44,32,114,49,50,98,10,32,32,109,111,118,32,114,49,50,44,32,114,100,120,10,32,32,99,109,112,32,114,98,120,44,32,114,100,105,10,32,32,106,110,101,32,46,119,114,105,116,101,95,100,105,103,105,116,115,10,32,32,114,101,116,10
func_95:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 1995]
  mov eax, 570
  call putsmallstr
  push qword 1
  lea rax, [rel vars + 8]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,10,112,117,116,115,109,97,108,108,115,116,114,58,10,32,32,108,101,97,32,114,98,120,44,32,91,114,97,120,32,43,32,114,99,120,93,10,32,32,99,109,112,32,101,98,120,44,32,111,117,116,112,117,116,95,98,117,102,46,108,101,110,10,32,32,106,98,101,32,46,115,117,102,102,105,99,105,101,110,116,95,99,97,112,97,99,105,116,121,10,32,32,109,111,118,32,101,98,120,44,32,101,97,120,10,32,32,109,111,118,32,114,49,50,44,32,114,100,105,10,32,32,99,97,108,108,32,102,108,117,115,104,10,32,32,109,111,118,32,114,100,105,44,32,114,49,50,10,10,32,32,46,115,117,102,102,105,99,105,101,110,116,95,99,97,112,97,99,105,116,121,58,10,32,32,109,111,118,32,114,100,120,44,32,114,115,105,10,32,32,109,111,118,32,114,115,105,44,32,114,100,105,10,32,32,108,101,97,32,114,100,105,44,32,114,100,120,91,114,99,120,93,10,32,32,109,111,118,32,101,99,120,44,32,101,97,120,10,32,32,114,101,112,32,109,111,118,115,98,10,32,32,109,111,118,32,114,115,105,44,32,114,100,120,10,32,32,109,111,118,32,101,99,120,44,32,101,98,120,10,32,32,114,101,116,10,10,112,117,116,98,105,103,115,116,114,58,10,32,32,116,101,115,116,32,101,99,120,44,32,101,99,120,10,32,32,106,122,32,46,110,111,102,108,117,115,104,10,32,32,99,97,108,108,32,102,108,117,115,104,10,32,32,46,110,111,102,108,117,115,104,58,10,32,32,109,111,118,32,114,98,120,44,32,114,115,105,10,32,32,109,111,118,32,114,115,105,44,32,114,100,105,10,32,32,109,111,118,32,101,100,120,44,32,101,97,120,10,32,32,109,111,118,32,101,97,120,44,32,48,120,48,50,48,48,48,48,48,52,10,32,32,109,111,118,32,101,100,105,44,32,49,10,32,32,115,121,115,99,97,108,108,10,32,32,109,111,118,32,114,115,105,44,32,114,98,120,10,32,32,114,101,116,10
func_96:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 2565]
  mov eax, 464
  call putsmallstr
  push qword 1
  lea rax, [rel vars + 8]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,10,102,108,117,115,104,58,10,32,32,109,111,118,32,101,97,120,44,32,48,120,48,50,48,48,48,48,48,52,10,32,32,109,111,118,32,101,100,105,44,32,49,10,32,32,109,111,118,32,101,100,120,44,32,101,99,120,10,32,32,115,121,115,99,97,108,108,10,32,32,120,111,114,32,101,99,120,44,32,101,99,120,10,32,32,114,101,116,10
func_97:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 3029]
  mov eax, 89
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,10,103,108,111,98,97,108,32,115,116,97,114,116,10,115,116,97,114,116,58,10,32,32,109,111,118,32,114,97,120,44,32,91,114,115,112,32,43,32,56,93,10,32,32,109,111,118,32,91,114,101,108,32,118,97,114,115,32,43,32,48,93,44,32,114,97,120,10,32,32,109,111,118,32,114,98,112,44,32,114,115,112,10,32,32,108,101,97,32,114,115,112,44,32,91,114,101,108,32,115,116,97,99,107,46,98,111,116,116,111,109,93,10,32,32,108,101,97,32,114,115,105,44,32,91,114,101,108,32,111,117,116,112,117,116,95,98,117,102,93,10,32,32,120,111,114,32,101,99,120,44,32,101,99,120,10,32,32,106,109,112,32,102,97,108,115,101,95,109,97,105,110,10,102,105,110,105,115,104,58,10
%xdefine STRING_DATA STRING_DATA,32,32,99,97,108,108,32,102,108,117,115,104,10
func_98:
  sub rbp, 8
  pop qword [rbp]
  lea rdi, [rel string_data + 3299]
  mov eax, 13
  call putsmallstr
  push qword [rbp]
  add rbp, 8
  ret
%xdefine STRING_DATA STRING_DATA,32,32,109,111,118,32,101,97,120,44,32,48,120,48,50,48,48,48,48,48,49,10,32,32,120,111,114,32,114,100,105,44,32,114,100,105,10,32,32,115,121,115,99,97,108,108,10,32,32,117,100,50,10,10,115,116,114,105,110,103,95,100,97,116,97,58,10,32,32,100,98,32,83,84,82,73,78,71,95,68,65,84,65,10,10,115,101,99,116,105,111,110,32,46,98,115,115,32,97,108,105,103,110,61,52,48,57,54,10,32,32,115,116,97,99,107,58,32,32,32,32,32,32,114,101,115,113,32,49,48,50,52,42,49,48,50,52,10,32,32,46,98,111,116,116,111,109,58,10,32,32,111,117,116,112,117,116,95,98,117,102,58,32,114,101,115,98,32,52,48,57,54,10,32,32,46,108,101,110,58,32,32,32,32,32,32,32,101,113,117,32,36,32,45,32,111,117,116,112,117,116,95,98,117,102,10,32,32,105,110,112,117,116,95,98,117,102,58,32,32,114,101,115,98,32,49,10,32,32,97,108,105,103,110,98,32,56,10,32,32,118,97,114,115,58,32,32,32,32,32,32,32,114,101,115,113,32,50,54,10
false_main:
  lea rdi, [rel string_data + 0]
  mov eax, 386
  call putsmallstr
  push qword 34
  push qword [rsp]
  pop rbx
  call putchar
  pop rbx
  call putchar
  lea rdi, [rel string_data + 386]
  mov eax, 2
  call putsmallstr
  lea rax, [rel func_81]
  push rax
  lea rax, [rel vars + 16]
  push rax
  pop rax
  pop rdx
  mov [rax], rdx
  push qword 0
  lea rax, [rel func_82]
  push rax
  lea rax, [rel func_92]
  push rax
  while_call
  pop rax
  lea rdi, [rel string_data + 1580]
  mov eax, 12
  call putsmallstr
  lea rax, [rel vars + 16]
  push rax
  pop rax
  push qword [rax]
  pop rax
  call rax
  lea rdi, [rel string_data + 1592]
  mov eax, 13
  call putsmallstr
  lea rax, [rel vars + 72]
  push rax
  pop rax
  push qword [rax]
  lea rax, [rel func_93]
  push rax
  cond_call
  lea rax, [rel vars + 112]
  push rax
  pop rax
  push qword [rax]
  lea rax, [rel func_94]
  push rax
  cond_call
  lea rax, [rel vars + 104]
  push rax
  pop rax
  push qword [rax]
  lea rax, [rel func_95]
  push rax
  cond_call
  lea rax, [rel vars + 88]
  push rax
  pop rax
  push qword [rax]
  lea rax, [rel func_96]
  push rax
  cond_call
  lea rax, [rel vars + 8]
  push rax
  pop rax
  push qword [rax]
  lea rax, [rel func_97]
  push rax
  cond_call
  lea rdi, [rel string_data + 3118]
  mov eax, 181
  call putsmallstr
  lea rax, [rel vars + 8]
  push rax
  pop rax
  push qword [rax]
  lea rax, [rel func_98]
  push rax
  cond_call
  lea rdi, [rel string_data + 3312]
  mov eax, 260
  call putsmallstr
  jmp finish

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

putchar:
  cmp ecx, output_buf.len
  jb .sufficient_capacity
  call flush
  .sufficient_capacity:
  mov rsi[rcx], bl
  add ecx, 1
  ret

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

flush:
  mov eax, 0x02000004
  mov edi, 1
  mov edx, ecx
  syscall
  xor ecx, ecx
  ret

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
  call flush
  mov eax, 0x02000001
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
