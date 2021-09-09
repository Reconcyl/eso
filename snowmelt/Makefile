all: mlton

.PHONY: mlton, smlnj, repl
mlton: main
smlnj: build_smlnj.cm *.sml
	ml-build $< Main.main main_smlnj
repl: build_smlnj.cm *.sml
	sml $<

main: build_mlton.mlb *.sml
	mlton -default-type int64 -output $@ $<

.PHONY: clean
clean:
	rm -f main
