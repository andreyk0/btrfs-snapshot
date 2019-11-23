build:
	stack build
	hlint src

test:
	stack test

clean:
	stack clean


.PHONY: exec clean build test
