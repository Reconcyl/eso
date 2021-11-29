# This was adapted from a GCC disassembly of an itoa-like function.
# I rewrote it to test that it worked and understand the implementation,
# and I ended up making a number of simplifications that the compiler missed.
# `putnum` was a result of translating the simplified version back into
# assembly, with adaptations for IO and to use the correct registers.

globl = bytearray(100)
pos = 0

class Pointer:
    def __init__(self, arr, offset): self.arr = arr; self.offset = offset
    def store(self, n): self.arr[self.offset] = n
    def do_offset(self, i): return Pointer(self.arr, self.offset + i)
    def __eq__(self, other):
        return self.arr == other.arr and self.offset == other.offset

def decompose(n: 128):
    return (n >> 64), n & ((1 << 64) - 1)

def write_digits(rdi):
    global pos
    rcx = rdi
    rdx = rdi
    rdi = 0 # xor
    r9 = 0xCCCCCCCCCCCCCCCD
    while True: # .L9
        rax = rdx
        rsi = rdx
        rdi += 1 # 32
        rdx, _ = decompose(rax * r9)
        rdx = rdx >> 3
        if (rsi > 9): continue
        break
    print("number of digits:", rdi)
    rax = rdi # 32
    pos += rax
    rsi = Pointer(globl, pos)
    r9 = 0xCCCCCCCCCCCCCCCD # redundant
    rdi -= 1 # 32
    rdi = ~rdi
    rdi = rsi.do_offset(rdi)
    while True: # .L11
        rax = rcx
        rsi = rsi.do_offset(-1) # pointer
        rdx, _ = decompose(rax * r9)
        rdx = rdx >> 3
        rax = rdx*5 # lea \
        rax += rax  #     /`- essentially these do rax = rdx*10
        rcx -= rax
        rcx += 48 # 32
        rsi.store(rcx)
        rcx = rdx
        if rsi != rdi: continue
        break
    print("result:", globl)
    print("new value of pos:", pos)

write_digits(31415926)
