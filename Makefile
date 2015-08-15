.PHONY = all clean build

all: build

deps:
	cabal configure
	cabal build

build:
	runhaskell Setup.lhs configure
	runhaskell Setup.lhs build
	# if you get the error "cannot satisfy -package-id" run 
	# $ ghc-pkg check 
	# which should give you a WARNING cache out of date.  
	# Then run
	# $ ghc-pkg recache
	# Which should fix the problem
