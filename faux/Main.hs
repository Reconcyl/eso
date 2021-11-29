{-# LANGUAGE LambdaCase, RebindableSyntax #-}

module Main (main) where

import Prelude
import Data.List (genericIndex)
import Data.Int (Int32)
import Data.Bits ((.&.), (.|.), complement)
import Data.Word (Word8)
import Data.Char (chr, ord)

import qualified Data.ByteString as BS
import qualified Data.Text as Text (pack)
import qualified Data.Text.Encoding as Text (encodeUtf8)

-- we need to provide this because of RebindableSyntax for some reason
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  a _ = a
ifThenElse False _ b = b

data Platform = PLinux | PMacOS

platform :: String -> Maybe Platform
platform "linux" = Just PLinux
platform "macos" = Just PMacOS
platform _ = Nothing

type Program = [Command]
data Command
  = Lit !Elem -- literals
  | Dup | Pop | Xch | Rot | Pck -- stack manipulation
  | Add | Sub | Mul | Div | Neg -- arithmetic
  | And | Or | Not -- bit manipulation
  | Gt | Eq -- comparison
  | Exec | CondExec | While -- functions
  | Store | Fetch -- variables
  | InChar | OutChar | OutNum | OutString !String | Flush -- IO
data Elem = Num !Int32 | Fn ![Command] | Var !Word8

instance Show Elem where
  show (Num i) = show i
  show (Fn _) = "[...]"
  show (Var i) = ['&', chr (ord 'a' + fromEnum i)]

falseToX64AsmCompiler :: Platform -> Program
falseToX64AsmCompiler plat =
  let
    -- builtins
    one = (:); (>>) = (.); nop = id
    push = one . Lit . Num
    pushBlock b = one $ Lit $ Fn $ b []
    dup = one Dup; pop = one Pop; xch = one Xch; rot = one Rot; pck = one Pck
    add = one Add; sub = one Sub; mul = one Mul; div_ = one Div; neg = one Neg
    dec n = inc (-n); inc n
      | n > 0 = push n >> add
      | n < 0 = push (-n) >> sub
      | otherwise = id
    bitAnd = one And; bitOr = one Or; bitNot = one Not
    eq = one Eq; gt = one Gt

    -- turn 0x....abcd into 0xcdab
    bswap16 = do
      dup; push 0xff; bitAnd; push 256; mul
      xch; push 0xff00; bitAnd; push 256; div_
      bitOr

    if_ b = pushBlock b >> one CondExec
    if_else t e = do
      dup; if_ $ do pop; t; push 1
      push 0; eq; if_ $ do e

    -- Takes a list of branches and constructs a switch statement based on the TOS.
    -- Each branch consists of:
    -- 1. The beginning and end of the range of numbers to be considered for this case.
    -- 2. The code to run for this case. The index into the range will be on top of the stack,
    --    unless there was only one element in the range (start == end).
    -- The branches must be in sorted order. An additional block can be passed,
    -- which will be run if none of the cases match.
    -- An block to run if none of the cases match.
    switch :: [((Int32, Int32), Program -> Program)] -> (Program -> Program) -> Program -> Program
    -- TODO: rewrite to use if/else chain, which is shorter
    switch cases fallback = go 0 cases where
      go offset [] = do pop; fallback
      go offset (((start, end), caseBody):rest)
        | offset >= start = error "cases are not in order"
        | start > end = error "case range is invalid"
        | start == end = do
          -- subtract the appropriate value from the TOS
          dec (start - offset)
          dup; if_else
            (go start rest) -- if the TOS is nonzero, try the next case
            (do pop; caseBody) -- if the TOS is zero, run this case
        | offset + 1 == start = do
          dup; push 0; xch; gt; xch
          dec 1
          dup; push (end - start); gt
          rot; bitOr
          if_else
            (go start rest) -- if the TOS is out of range, try the next case
            caseBody -- if the TOS is in range, run this case
        | otherwise = error "unsupported case"

    while c b = pushBlock c >> pushBlock b >> one While

    refVar v =
      let idx = ord v - ord 'a' in
      one . Lit . Var . toEnum $ idx
    getVar v = refVar v >> one Fetch
    setVar v = refVar v >> one Store
    defineProc v b = pushBlock b >> setVar v
    proc v = getVar v >> one Exec

    inChar = one InChar
    outChar = one OutChar
    outNum = one OutNum
    out = one . OutString
    cr = out "\n"

    -- pop N and remove N elements from the stack
    popN = do
      while dup (do xch; pop; dec 1)
      pop

    -- variables used:
    -- c  procedure to compile a function body (shared between bracketed fns and main)
    -- d  cumulative decimal number
    -- e  whether the last character was a digit
    -- f  number of functions finished compiling so far
    -- i  implementation detail of c
    -- j  whether the '^' command (read char) is used
    -- o  whether the ',' command (write char) is used
    -- n  whether the '.' command (write decimal) is used
    -- l  whether the '"' command (write string) is used
    -- k  how many bytes have been used in strings so far
    -- b  whether the 'ß' command (flush) is used (or we have compiled another output function which calls this)
    -- q  used during function compilation state:
    --      q=3 immediately after a "
    --      q=2 after 3
    --      q=1 immediately after a '
    --      q=0 after a normal command

    bufsz = 4096

    -- unlike the interpreter below, values here are 64 bit so we can accommodate pointers
    writeDataSection = out $ unlines
      ["section .bss align=4096",
       "  stack:      resq 1024*1024",
       "  .bottom:",
       "  output_buf: resb " ++ show bufsz,
       "  .len:       equ $ - output_buf",
       "  input_buf:  resb 1",
       "  alignb 8",
       "  vars:       resq 26"]

    sc_write = case plat of
      PLinux -> "1"
      PMacOS -> "0x02000004"

    sc_read = case plat of
      PLinux -> "0"
      PMacOS -> "0x02000003"

    sc_exit = case plat of
      PLinux -> "60"
      PMacOS -> "0x02000001"

    -- write ASM that defines a function which flushes the STDOUT buffer
    writeFlushFn = out $ unlines
      ["flush:",
       "  mov eax, " ++ sc_write,
       "  mov edi, 1", -- stdout
       -- rsi already has the correct value
       "  mov edx, ecx", -- data len
       "  syscall", -- clobbers at least rdx, probably also r11, maybe r8-r10 as well; I'm not sure
       -- ignore the syscall return value; there's nothing we can do if it fails
       "  xor ecx, ecx", -- reset the buffer length to 0
       "  ret"]

    -- invariants preserved between instructions:
    -- 1. rsp is the data stack pointer
    -- 2. rbp is the return stack pointer
    -- 3. rax, rbx, rdx, rdi, r11 store arbitrary values
    -- 4. rsi points to the start of the output buffer
    -- 5. rcx contains the number of bytes filled in the output buffer
    writeStart = do
      out $ unlines
        ["global start",
         "start:",
         -- assign `a` = argv
         "  mov rax, [rsp + 8]",
         "  mov [rel vars + 0], rax",
         "  mov rbp, rsp",
         "  lea rsp, [rel stack.bottom]",
         "  lea rsi, [rel output_buf]",
         "  xor ecx, ecx",
         "  jmp false_main",
         "finish:"]
      -- only flush if the program could have performed output
      -- (otherwise this function doesn't even exist)
      getVar 'b'; if_ $ do out $ unlines ["  call flush"]
      -- exit
      out $ unlines
        ["  mov eax, " ++ sc_exit,
         "  xor rdi, rdi",
         "  syscall",
         -- in case the exit fails for some reason
         "  ud2"]

    -- pop a complemented character code from the stack
    -- and write ASM that pushes the charater code onto the stack
    writeIntLiteral = do out "  push qword "; outNum; cr

    -- pop a function ID from the stack
    -- and write ASM that pushes the function's address to the stack
    writePushFn = do
      out "  lea rax, [rel func_"; outNum; out "]"; cr
      out "  push rax"; cr

    -- pop a variable ID and write ASM that pushes the corresponding variable
    writePushVarRef = do
      push 8; mul
      out "  lea rax, [rel vars + "; outNum; out "]"; cr
      out "  push rax"; cr

    -- write ASM that duplicates the top stack element
    writeDup = out $ unlines ["  push qword [rsp]"]

    -- write ASM that removes the top stack element
    writePop = out $ unlines ["  pop rax"]

    -- write ASM that swaps the top two stack elements
    writeXch = out $ unlines
       -- read two words from the stack
      ["  movdqu xmm0, [rsp]",
       -- swap the order
       "  shufpd xmm0, xmm0, 1",
       -- write them back
       "  movdqu [rsp], xmm0"]

    -- write ASM that rotates the top three stack elements
    writeRot = out $ unlines
      ["  mov rax, [rsp + 16]",
       "  movdqu xmm0, [rsp]",
       "  movdqu [rsp + 8], xmm0",
       "  mov [rsp], rax"]

    -- write ASM that performs the 'pick' operation
    writePck = out $ unlines
      ["  pop rax",
       "  push qword [rsp + 8*rax]"]

    -- write ASM that computes the height of the stack and pushes it
    writePushStackLen = out $ unlines
      ["  lea rax, [rel stack.bottom]",
       "  sub rax, rsp",
       "  sar eax, 3",
       "  push rax"]

    -- write ASM that performs certain kinds of
    -- mathematical operators on the top stack elements
    writeAdd = out $ unlines ["  pop rax", "  add [rsp], rax"]
    writeSub = out $ unlines ["  pop rax", "  sub [rsp], rax"]
    writeOr  = out $ unlines ["  pop rax",  "  or [rsp], rax"]
    writeAnd = out $ unlines ["  pop rax", "  and [rsp], rax"]
    writeMul = out $ unlines ["  pop rax", "  imul rax, [rsp]", "  mov [rsp], rax"]
    writeDiv = out $ unlines ["  pop rbx", "  pop rax", "  cqo", "  idiv rbx", "  push rax"]
    writeNeg = out $ unlines ["  neg qword [rsp]"]
    writeNot = out $ unlines ["  not qword [rsp]"]

    writeGt = out $ unlines
      ["  pop rax",
       "  xor ebx, ebx",
       "  cmp rax, [rsp]",
       "  setl bl",
       "  neg rbx",
       "  mov [rsp], rbx"]

    -- write ASM that determines whether the top two stack values are equal
    writeEq = out $ unlines
      ["  pop rax",
       "  xor ebx, ebx",
       "  cmp rax, [rsp]",
       "  sete bl",
       "  neg rbx",
       "  mov [rsp], rbx"]

    -- write ASM that pops a function pointer from the stack and calls it
    writeCall = out $ unlines
      ["  pop rax",
       "  call rax"] -- the callee will handle moving the return address
                     -- to a safer location so the stack can be used

    -- Write a NASM macro directive that has the effect of popping a function
    -- pointer and a condition, then calling the function if the condition is
    -- nonzero.
    writeDefineCondCallMacro = out $ unlines
      ["%macro cond_call 0",
       "  pop rbx",
       "  pop rax",
       "  test rax, rax",
       "  jz %%end",
       "  call rbx",
       "  %%end:",
       "%endmacro"]

    -- write ASM that invokes this macro
    writeCondCall = out $ unlines ["  cond_call"]

    -- write a NASM macro directive that has the effect of popping a
    -- block function and a condition and then running the block as
    -- long as the condition pushes a truthy value
    writeDefineWhileCallMacro = out $ unlines
      ["%macro while_call 0",
       -- pop two functions
       "  movdqu xmm0, [rsp]",
       "  add rsp, 16",
       -- copy the high one (condition function) into rax
       "  pextrq rax, xmm0, 1",
       -- save the functions in the OS stack
       "  sub rbp, 16",
       "  movdqu [rbp], xmm0",
       -- first time checking the condition
       "  call rax",
       "  pop rax",
       "  test rax, rax",
       "  jz %%end",
       -- if it holds, enter a do-while style loop:
       "%%start:",
       -- run the body...
       "  call [rbp]",
       -- ... then check the condition again
       "  call [rbp+8]",
       "  pop rax",
       "  test rax, rax",
       "  jnz %%start",
       "%%end:",
       -- discard the condition pointers
       "  add rbp, 16",
       "%endmacro"]

    -- write ASM that invokes this macro
    writeWhileCall = out $ unlines ["  while_call"]

    -- write ASM that pops a variable pointer followed by an element
    -- and stores the element at that pointer
    writeStore = out $ unlines
      ["  pop rax",
       "  pop rdx",
       "  mov [rax], rdx"]

    -- write ASM that pops a variable pointer and reads the variable at that pointer
    writeLoad = out $ unlines
      ["  pop rax",
       "  push qword [rax]"]

    -- write ASM that defines a function which reads a byte from STDIN and
    -- stores it in `rdi`, or -1 on EOF/error
    writeGetcharFn = out $ unlines
      ["getchar:",
       "  mov rbx, rsi", -- save output buffer
       "  mov r12d, ecx", -- save output buffer usage
       "  mov eax, " ++ sc_read,
       "  xor edi, edi", -- 0 = stdin
       "  lea rsi, [rel input_buf]", -- input buffer
       "  mov edx, 1", -- number of bytes to read
       "  syscall",
       -- restore output buffer
       "  mov rsi, rbx",
       "  mov ecx, r12d",
       -- return -1 if the return value is <= 0
       "  test eax, eax",
       "  jbe .eof",
       -- otherwise, read it from the buffer
       "  movzx edi, byte [rel input_buf]",
       "  ret",
       "  .eof:",
       "  sub rdi, 1", -- we know edi=0, so this sets edi to -1 compactly
       "  ret"]

    writeInChar = do push 1; setVar 'j'; out $ unlines ["  call getchar", "  push rdi"]

    -- write ASM that defines a function which writes the low byte of `rbx`
    -- to the buffer, flushing if necessary
    writePutcharFn = out $ unlines
      ["putchar:",
       "  cmp ecx, output_buf.len",
       "  jb .sufficient_capacity",
       "  call flush",
       "  .sufficient_capacity:",
       "  mov rsi[rcx], bl",
       "  add ecx, 1",
       "  ret"]

    -- write ASM that calls this function with a value popped from the stack
    writeOutChar = do push 1; setVar 'o'; out $ unlines ["  pop rbx", "  call putchar"]

    writeInitStringMacro = do
      out "%define STRING_DATA "
      push 34; dup; outChar; outChar -- quotes are not handled well
      cr

    -- write ASM that defines two functions which are used to output static strings.
    -- the first one should be used when the string is short, and attempts to copy
    -- to the buffer (flushing if necessary); the second writes unconditionally.
    writePutstrFns = out $ unlines
      -- both functions are called with rdi = string pointer, eax = length
      ["putsmallstr:",
       -- compute the new buffer usage, and flush if this would be too large
       -- (rbx is used later)
       "  lea rbx, [rax + rcx]",
       "  cmp ebx, output_buf.len",
       "  jbe .sufficient_capacity",
       "  mov ebx, eax",
       "  mov r12, rdi", -- preserve rdi (I lost a lot of time to this bug...)
       "  call flush",
       "  mov rdi, r12",
       "",
       "  .sufficient_capacity:",
       -- set up registers for the copy
       "  mov rdx, rsi",
       "  mov rsi, rdi",      -- src = string
       "  lea rdi, rdx[rcx]", -- dst = &output_buffer[used]
       "  mov ecx, eax",      -- len = length
       -- memcpy(dst, src, len);
       -- not the fastest way to do it, but it's simple
       "  rep movsb",
       "  mov rsi, rdx", -- restore output buffer
       "  mov ecx, ebx", -- new used
       "  ret",
       "",
       "putbigstr:",
       -- always flush the output buffer unless it is empty
       "  test ecx, ecx",
       "  jz .noflush",
       "  call flush",
       "  .noflush:",
       -- system call to write the string
       "  mov rbx, rsi",
       "  mov rsi, rdi", -- data
       "  mov edx, eax", -- data len
       "  mov eax, " ++ sc_write,
       "  mov edi, 1", -- stdout
       "  syscall",
       "  mov rsi, rbx", -- restore output buffer
       "  ret"]

    -- pop a complemented offset from the stack
    -- and write ASM that sets up `rdi` to point into string_data at that offset
    writeOutStrLiteralPart1 = do
      bitNot
      out "  lea rdi, [rel string_data + "; outNum; out "]"; cr

    -- pop a complemented length from the stack
    -- and write ASM that loads that length into `eax` and calls the appropriate function
    writeOutStrLiteralPart2 = do
      bitNot
      dup; out "  mov eax, "; outNum; cr
      push (bufsz `div` 4); gt
      if_else
        (do out "  call putbigstr"; cr)
        (do out "  call putsmallstr"; cr)

    -- write ASM that defines a function which writes the contents of `rdx` to the
    -- output buffer as an unsigned 64-bit decimal integer, flushing if necessary
    writePutnumFn = out $ unlines
      ["putnum:",
       "  mov r12, rdx", -- save a copy of the input
       "  xor ebx, ebx", -- total digits in the number
       "  mov r13, 0xCCCCCCCCCCCCCCCD", -- multiplicative inverse constant for fast division by 10

       -- count the number of digits needed
       -- (this handles 0 correctly by only checking at the end)
       "  .count_digits:",
         "  mov rax, rdx",
         "  mov rdi, rdx",
         "  add ebx, 1",
         "  mul r13", -- rdx = high 64 bits of (rax * r13)
         "  shr rdx, 3",
         "  cmp rdi, 9",
         "  ja .count_digits",

       -- compute the new buffer usage, and flush if this would be too large
       "  lea rax, [rbx + rcx]",
       "  cmp eax, output_buf.len",
       "  jbe .sufficient_capacity",
         "  call flush",
       "  .sufficient_capacity:",

       -- rdi: address of the last byte to write (first in the array)
       -- rbx: address of the first byte to write (last in the array)
       "  lea rdi, [rsi + rcx]",
       "  add ecx, ebx",
       "  lea rbx, [rsi + rcx]",

       -- write the digits to the buffer
       "  .write_digits:",
         "  mov rax, r12",
         "  sub rbx, 1",
         "  mul r13", -- rdx = high 64 bits of (rax * r13)
         "  shr rdx, 3",
         "  lea rax, [rdx + rdx*4]",
         "  add rax, rax", -- rax = rdx * 10
         "  sub r12, rax",
         "  add r12d, 48",
         "  mov [rbx], r12b",
         "  mov r12, rdx",
         "  cmp rbx, rdi",
         "  jne .write_digits",

       "  ret"]

    -- write ASM that calls this function with a value popped from the stack
    writeOutNum = do push 1; setVar 'n'; out $ unlines ["  pop rdx", "  call putnum"]

    -- embed the value popped from the stack into the ASM a big endian 16 bit integer
    writeInlineAsmWord = do bswap16; out "  dw "; outNum; cr

    -- given a number on the stack:
    -- - if q=4, treat its complement as the 2nd byte of a 2-byte command (ø or ß) and set q=0
    -- - if q=3, treat its complement as the length of the string and set q=2
    -- - if q=2, treat its complement as the offset of the string and set q=0
    -- - if q=1, compile its complement as a character literal and set q=0
    -- - if q=0:
    --   - if the character is negative, treat its complement as a character and compile
    --     the instruction with the corresponding semantics
    --     as long as it is not [ or ]
    --   - if it is positive, treat it as a function reference and compile the relevant function
    --   - assume the number is not zero
    dispatchCompile = do
      let c2i c = toEnum $ fromEnum c :: Int32
      let a `to` b = (c2i a, c2i b)
      let exactly c = (c2i c, c2i c)
      let a ==> b = (a, b)
      let default_case c = c
      getVar 'q'; switch
        [ exactly 1 ==> do bitNot; writeIntLiteral; push 0; setVar 'q'
        , exactly 2 ==> do writeOutStrLiteralPart2; push 0; setVar 'q'
        , exactly 3 ==> do writeOutStrLiteralPart1; push 2; setVar 'q'
        , exactly 4 ==> do dispatchCompileMultiByte; push 0; setVar 'q'
        ] $ default_case $ do
      dup; push 0; gt; if_else writePushFn $ do
      bitNot
      switch
        [ exactly '!' ==> writeCall
        , exactly '"' ==> do push 1; setVar 'l'; push 3; setVar 'q'
        , exactly '#' ==> writeWhileCall
        , exactly '$' ==> writeDup
        , exactly '%' ==> writePop
        , exactly '&' ==> writeAnd
        , exactly '\'' ==> do push 1; setVar 'q'
        , exactly '*' ==> writeMul
        , exactly '+' ==> writeAdd
        , exactly ',' ==> writeOutChar
        , exactly '-' ==> writeSub
        , exactly '.' ==> writeOutNum
        , exactly '/' ==> writeDiv
        , '0'`to`'9' ==> do getVar 'd'; push 10; mul; add; setVar 'd'
        , exactly ':' ==> writeStore
        , exactly ';' ==> writeLoad
        , exactly '=' ==> writeEq
        , exactly '>' ==> writeGt
        , exactly '?' ==> writeCondCall
        , exactly '@' ==> writeRot
        , exactly 'L' ==> writePushStackLen
        , exactly '\\' ==> writeXch
        , exactly '^' ==> writeInChar
        , exactly '_' ==> writeNeg
        , exactly '`' ==> do getVar 'd'; writeInlineAsmWord; push 0; setVar 'd'
        , 'a'`to`'z' ==> writePushVarRef
        , exactly '|' ==> writeOr
        , exactly '}' ==> do getVar 'd'; writeIntLiteral; push 0; setVar 'd'
        , exactly '~' ==> writeNot
        , exactly 0xc3 ==> do push 4; setVar 'q' -- both ø and ß start with c3 in utf8
        ] (default_case $ do out "  ; invalid opcode"; cr)

    dispatchCompileMultiByte = do
      bitNot
      dup
      push 0xb8; eq; if_ $ do writePck
      push 0x9f; eq; if_ $ do out $ unlines ["  call flush"]; push 1; setVar 'b'

    compileFunctionBody = do
      -- search backward on the stack until a 0 is found
      push 1
      while (do dup; pck) (inc 1)
      dup; setVar 'i' -- save in variable i
      -- compile instructions from the 0 to the end of the stack
      while (do dec 1; dup) (do dup; pck; dispatchCompile)
      -- remove these instructions from the stack along with the 0
      getVar 'i'
      popN
      pop

    compiler = do
      writeDefineWhileCallMacro
      cr
      writeDefineCondCallMacro
      cr
      writeInitStringMacro
      cr
      defineProc 'c' compileFunctionBody
      -- main compilation loop:
      push 0
      -- iterate over stdin
      while (do inChar; bitNot; dup) $ do
        -- complement of each STDIN character is on the stack

        -- determine whether to insert a close brace at the end of an integer literal:
        -- (1) was the last character a digit?
        getVar 'e'
        -- (2) is this character not a digit? (update `e`)
        push 1; pck; inc $ toEnum $ ord '9' + 1 -- digit range: 0-9
        dup; push 0; bitNot; gt
        xch; push 9; gt; bitNot; bitAnd
        dup; setVar 'e'
        bitNot
        -- (3) is this character not a backtick?
        push 2; pck; push 96; bitNot; eq; bitNot
        -- if all of these are true, insert it
        bitAnd; bitAnd; if_ $ do
          push 125; bitNot; xch
        -- note that this means we neglect to compile integer literals at the end
        -- of the program, but this is fine, since those are not observable anyway

        -- replace the character with 0 if its current value is ~91 (~'[')
        dup; push 91; bitNot; eq; bitNot; bitAnd
        -- start compiling a function if its current value is ~93 (~']')
        dup; push 93; bitNot; eq; if_ $ do
          -- ignore the top of the stack
          pop
          -- emit function label
          getVar 'f'; inc 1; dup; setVar 'f'
          out "func_"; outNum; out ":"; cr
          -- save the return address onto the return stack
          out $ unlines
            ["  sub rbp, 8",
             "  pop qword [rbp]"]
          -- compile the function body
          proc 'c'
          -- restore the return address
          out $ unlines
            ["  push qword [rbp]",
             "  add rbp, 8",
             "  ret"]
          -- push the function ID onto the stack
          getVar 'f'
        -- read an additional character if the current value is ~39 ( ' )
        dup; push 39; bitNot; eq; if_ $ do inChar; bitNot
        -- read many additional characters if the current value is ~34 ( " )
        dup; push 34; bitNot; eq; if_ $ do
          push 0
          -- use NASM xdefine to add characters to the STRING_DATA macro
          out "%xdefine STRING_DATA STRING_DATA"
          while (do inChar; dup
                    dup; push 34; eq
                    -- treat EOF as a closing quote
                    xch; push 0; bitNot; eq
                    bitOr; bitNot) $ do
            out ","
            outNum
            inc 1
          cr
          pop -- remove garbage value, now string length is on top of the stack
          dup; getVar 'k'; dup; rot; add; setVar 'k' -- add to k, leaving the old value on the stack
          bitNot; xch; bitNot

        -- ignore characters until closing } if the current value is ~123 ('{')
        dup; push 123; bitNot; eq; if_ $ do
          pop
          while (do inChar
                    dup; push 125; eq
                    -- treat EOF as a closing brace
                    xch; push 0; bitNot; eq
                    bitOr; bitNot) $ do nop
      pop
      out "false_main:"; cr
      -- compile main
      proc 'c'
      out "  jmp finish"; cr
      getVar 'j'; if_ $ do cr; writeGetcharFn
      getVar 'o'; if_ $ do cr; writePutcharFn; push 1; setVar 'b'
      getVar 'n'; if_ $ do cr; writePutnumFn; push 1; setVar 'b'
      getVar 'l'; if_ $ do cr; writePutstrFns; push 1; setVar 'b'
      getVar 'b'; if_ $ do cr; writeFlushFn
      cr
      writeStart
      cr
      out $ unlines
        ["string_data:",
         "  db STRING_DATA"]
      cr
      writeDataSection

    -- rewrite consecutive strings into a single string
    simplify [] = []
    simplify (OutString a : OutString b : cs) =
      simplify (OutString (a ++ b) : cs)
    simplify (Lit (Fn f) : cs) = Lit (Fn (simplify f)) : simplify cs
    simplify (c : cs) = c : simplify cs

  in simplify $ compiler []

type Stack = [Elem]

data InputStream = InputStream !BS.ByteString !Int deriving (Show)

initInput :: String -> InputStream
initInput s = InputStream (Text.encodeUtf8 $ Text.pack s) 0

nextInput :: InputStream -> (Int32, InputStream)
nextInput ii@(InputStream bs idx)
  | idx < BS.length bs = (byteToWord (BS.index bs idx), InputStream bs (idx + 1))
  | otherwise          = (-1, ii)
  where
    byteToWord :: Word8 -> Int32
    byteToWord = toEnum . fromEnum

data State = State !Stack ![Elem] !InputStream !String deriving (Show)

showPgm :: Program -> String
showPgm = \pgm -> reverse $ go False pgm "" where
  go :: Bool -> [Command] -> [Char] -> [Char]
  go _ [] res = res
  go isNum (cmd:cmds) acc = go isNum' cmds (f acc) where
    isNum' = case cmd of Lit (Num _) -> True; _ -> False
    f = case cmd of
      Lit (Num n) | isNum     -> cc (show n) . c ' '
                  | otherwise -> cc (show n)
      Lit (Fn cs)             -> c ']' . go False cs . c '['
      Lit (Var v)             -> (chr (ord 'a' + fromEnum v) :)
      Dup -> c '$'; Pop -> c '%'; Xch -> c '\\'; Rot -> c '@'; Pck -> c 'ø'
      Add -> c '+'; Sub -> c '-'; Mul -> c '*'; Div -> c '/'; Neg -> c '_'
      And -> c '&'; Or -> c '|'; Not -> c '~'
      Gt -> c '>'; Eq -> c '='
      Exec -> c '!'; CondExec -> c '?'; While -> c '#'
      Store -> c ':'; Fetch -> c ';'
      InChar -> c '^'; OutChar -> c ','; OutNum -> c '.'; OutString s -> c '"' . strContents s . c '"'; Flush -> c 'ß'
      where c = (:)
            cc = (++) . reverse
            haveLineBreaks = True
            strContents
              | haveLineBreaks = cc
              | otherwise = (++) . concatMap (\case '\n' -> "\",01\""; c -> [c]) . reverse

instance Show Command where show = showPgm . pure; showList = (++) . showPgm

runMany :: [Command] -> State -> State
runMany [] = id
runMany (p:ps) = runMany ps . run p

run :: Command -> State -> State
run = \case
  Lit e -> changeStack (e:)
  Dup -> changeStack $ \es -> head es:es
  Pop -> changeStack $ \es -> tail es
  Xch -> changeStack $ \(a:b:es) -> b:a:es
  Rot -> changeStack $ \(a:b:c:es) -> c:a:b:es
  Pck -> changeStack $ \(Num n:es) -> genericIndex es n : es
  Add -> arith2 (+)
  Sub -> arith2 (-)
  Mul -> arith2 (*)
  Div -> arith2 div
  Neg -> arith1 negate
  And -> arith2 (.&.)
  Or  -> arith2 (.|.)
  Not -> arith1 complement
  Gt  -> arith2 $ \a b -> if a > b then -1 else 0
  Eq  -> arith2 $ \a b -> if a == b then -1 else 0

  Exec     -> \st ->
                let (Fn f, st') = pop st in
                runMany f st'
  CondExec -> \st ->
                let (Fn f, st') = pop st in
                let (e, st'') = pop st' in
                if truthy e then runMany f st'' else st''
  While    -> \st ->
                let (Fn b, st') = pop st in
                let (Fn c, st'') = pop st' in
                let go st = let (e, st') = pop (runMany c st) in
                              if truthy e then go $ runMany b st' else st'
                in go st''

  Store -> \st ->
    let (Var v, st') = pop st in
    let (e, State es vs ii oo) = pop st' in
    State es (setIdx (fromEnum v) e vs) ii oo

  Fetch -> \st ->
    let (Var v, State es vs ii oo) = pop st in
    State (genericIndex vs v : es) vs ii oo

  InChar ->
    \(State es vs ii oo) ->
      let (i, ii') = nextInput ii in
      State (Num i:es) vs ii' oo

  OutChar     -> \(State (Num n:es) vs ii oo) -> State es vs ii (chr (fromEnum n):oo)
  OutNum      -> \(State (Num n:es) vs ii oo) -> State es vs ii (reverse (show n) ++ oo)
  OutString s -> \(State es vs ii oo) -> State es vs ii (reverse s ++ oo)
  Flush       -> id -- doesn't make sense to flush with this implementation

  where
    setIdx :: Int -> a -> [a] -> [a]
    setIdx n x' (x:xs)
      | n == 0    = x' : xs
      | n > 0     = x : setIdx (n - 1) x' xs
      | otherwise = error "n cannot be negative"
    truthy (Num 0) = False
    truthy _ = True
    pop (State (e:es) vs ii oo) = (e, State es vs ii oo)
    changeStack f (State es vs ii oo) = State (f es) vs ii oo
    arith1 f = changeStack $ \(Num a:      es) -> Num (f   a) : es
    arith2 f = changeStack $ \(Num a:Num b:es) -> Num (f b a) : es

execute :: Program -> String -> String
execute pgm input =
  let initState = State [] (replicate 26 $ Num 0) (initInput input) "" in
  let finalState = runMany pgm initState in
  case finalState of
    State _ _ _ oo -> reverse oo

main :: IO ()
main = do
  let Just plat = platform "macos"
      comp = falseToX64AsmCompiler plat
      comp' = show comp
  writeFile "faux.f" comp'
  let asm = execute comp comp'
  writeFile "faux.asm" asm
