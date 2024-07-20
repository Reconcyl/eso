Find3: Find3.hs
	ghc -O Find3.hs

.PHONY: lazier_changes
lazier_changes:
	bash -c 'diff -U2 <(curl "https://raw.githubusercontent.com/msullivan/LazyK/master/lazier.scm") lazier.scm | vim -'

.PHONY: clean
clean:
	rm Find3 Find3.hi Find3.o
