CABAL ?= cabal

c:; @$(CABAL) clean

b:v2;

r:b; $(CABAL) v2-repl

v2:; $(CABAL) v2-build

.DEFAULT_GOAL := b 

.PHONY : c b r v2 