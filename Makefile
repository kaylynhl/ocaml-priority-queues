.PHONY: build test fmt check

build:
	dune build

test:
	dune test

fmt:
	dune fmt

check:
	dune build
	dune test
