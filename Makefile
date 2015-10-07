EXE=4hire
CABAL=Cabal-1.22.4.0
ARCH=x86_64-linux

run:
		stack build && .stack-work/dist/$(ARCH)/$(CABAL)/build/$(EXE)/$(EXE)

clean:
		stack clean

test:
	  stack test
