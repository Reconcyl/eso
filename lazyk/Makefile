Find3: Find3.hs
	ghc -O Find3.hs

.PHONY: lazier_changes
lazier_changes:
	bash -c 'diff -U2 <(curl "https://raw.githubusercontent.com/msullivan/LazyK/master/lazier.scm") lazier.scm | vim -'

CHECK ?= stdlib.scm ./examples

.PHONY: check
check:
	@find $(CHECK) -name '*.scm' -print0 | \
		xargs -0 -P0 -n1 ./check.sh

.PHONY: clean
clean:
	rm Find3 Find3.hi Find3.o
