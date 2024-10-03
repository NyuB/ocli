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
	# Copy the executables into installation directory
	cp _build/install/default/bin/rebase_edit $(INSTALL_ROOT)/rebase_edit
	cp rebase/newbase $(INSTALL_ROOT)/newbase
	# Make the installed files writable to allow future deletion or replacement
	chmod +w $(INSTALL_ROOT)/rebase_edit
	chmod +w $(INSTALL_ROOT)/newbase
