GHC_OPTIONS ?= -O
EXE_FORMAT ?= macho64

.PHONY: default
default: verify_bootstrap

Main: Main.hs
	ghc $(GHC_OPTIONS) $< -o $@

faux.f faux.asm: Main
	./$<

faux.o: faux.asm
	nasm -f $(EXE_FORMAT) $<

faux: faux.o
	ld -static $< -o $@

verify_bootstrap: faux.f faux.asm faux
	./faux <faux.f >faux1.asm
	diff -q faux.asm faux1.asm
	@echo "Bootstrap was successful."

.PHONY: clean
clean:
	rm -rf Main Main.o Main.hi faux.o faux1.asm faux

.PHONY: clean_all
clean_all: clean
	rm -rf faux.f faux.asm
