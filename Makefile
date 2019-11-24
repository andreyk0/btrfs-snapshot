build:
	stack build
	hlint src

clean:
	stack clean

.PHONY: exec clean build test
