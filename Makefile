.PHONY: build default demo fmt test test-promote
INSTALL_ROOT=~/bin

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

install-rebase: build
	# Copy the executable into installation directory
	cp _build/install/default/bin/rebase_edit $(INSTALL_ROOT)/rebase_edit
	# Make the installed file writable to allow future deletion or replacement
	chmod +w $(INSTALL_ROOT)/rebase_edit
