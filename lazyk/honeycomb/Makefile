.PHONY: all
all: Memory Honeycomb

Memory: Memory.hs
	ghc Memory.hs

Honeycomb: Honeycomb.hs
	ghc -O2 -Wall Honeycomb.hs

.PHONY: clean
clean:
	rm Memory Memory.hi Memory.o Honeycomb Honeycomb.hi Honeycomb.o
