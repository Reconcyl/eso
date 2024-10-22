.PHONY: all
all: exec ppx

exec: exec.c
	zig cc -Wall -Wextra -Wpedantic -O3 $< -o $@

ppx: ppx.zig
	zig build-exe -O ReleaseSafe -fsingle-threaded $<

.PHONY: clean
clean:
	rm -rf exec ppx ppx.o zig-cache
