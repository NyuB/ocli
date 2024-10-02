.PHONY: default test test-promote fmt demo

default: fmt test build

test:
	dune test

test-promote:
	-dune test --auto-promote

fmt:
	-dune fmt

build:
	dune build

demo:
	dune exec tty_demo
