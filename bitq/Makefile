GHC := stack ghc --
AstInterpreter: AstInterpreter.hs
	$(GHC) -O3 $< -o $@ -outputdir target

.PHONY: clean
clean:
	rm -rf target AstInterpreter
