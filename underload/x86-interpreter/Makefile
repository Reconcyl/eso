main: main.o
	ld $< -o $@

main.o: main.asm
	nasm -f elf64 $< -o $@

main.hex: main.asm
	nasm -f bin $< -o /dev/stdout | xxd >$@

.PHONY: clean
clean:
	rm main main.o
