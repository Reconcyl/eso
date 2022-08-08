MLTON = mlton
SMLNJ = sml

main: main.sml
	$(MLTON) $<

.PHONY: repl
repl: main.sml
	$(SMLNJ) not-mlton.sml main.sml
