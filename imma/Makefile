.PHONY: all
all: exec ppx

exec: exec.s
	zig cc -Wno-unused-command-line-argument $< -o $@

ppx: ppx.zig
	zig build-exe -O ReleaseSafe -fsingle-threaded $<

.PHONY: clean
clean:
	rm -rf exec ppx zig-cache
