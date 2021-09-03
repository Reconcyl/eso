all: mlton

.PHONY: mlton, repl
mlton: main
repl:
	sml main.sml

main: build_mlton.mlb *.sml
	mlton -output $@ $<

.PHONY: clean
clean:
	rm -f main
