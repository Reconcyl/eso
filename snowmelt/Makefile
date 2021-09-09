all: mlton

.PHONY: mlton, repl
mlton: main
repl: build_smlnj.cm *.sml
	sml $<

main: build_mlton.mlb *.sml
	mlton -default-type int64 -output $@ $<

.PHONY: clean
clean:
	rm -f main
