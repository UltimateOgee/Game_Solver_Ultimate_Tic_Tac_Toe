# Makefile for Ultimate Tic Tac Toe

# Restricted words that can't be used 
.PHONY: build init test clean doc deploy stage

build:
		ghc --make -O -o tictacgo Main.hs

all: build test

clean:
	rm -f tictacgo
	rm -f *.hi
	rm -f *.o

setup:
	cabal install ansi-terminal
