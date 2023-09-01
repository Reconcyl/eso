.PHONY: all
all: main

target/compiler.fut: compiler.fut
	@mkdir -p target
	cp $< target

target/compiler.h: target/compiler.c
target/compiler.c: target/compiler.fut
	futhark c --library $<

target/compiler.o: target/compiler.c
	$(CC) $(CCFLAGS) $< -c -o $@

main: main.zig target/compiler.h target/compiler.o
	zig build-exe -lc -Itarget main.zig target/compiler.o

.PHONY: clean
clean:
	rm -rf main target
