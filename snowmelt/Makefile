all: mlton

.PHONY: mlton, repl
mlton: main
repl:
	sml main.sml

main: main.sml
	mlton main.sml

.PHONY: clean
clean:
	rm -f main
