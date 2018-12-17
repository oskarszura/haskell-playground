.PHONY: install
install:
	cabal install wai warp
	cabal install utf8-string

.PHONY: all
all:
	ghc -o webserver webserver.hs
