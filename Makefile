.PHONY: build default demo fmt test test-promote
INSTALL_ROOT=~/bin

default: fmt test build doc

test:
	dune test

test-promote:
	-dune test --auto-promote

fmt:
	-dune fmt

fmt-check:
	dune build @fmt

build:
	dune build

demo:
	dune exec demo_tty

doc:
	dune build @doc
	dune build @doc-private

install-newbase: build
	# Copy the executables into installation directory
	cp _build/install/default/bin/rebase_edit $(INSTALL_ROOT)/rebase_edit
	cp rebase/newbase.t/newbase $(INSTALL_ROOT)/newbase
	# Make the installed files writable to allow future deletion or replacement
	chmod +w $(INSTALL_ROOT)/rebase_edit
	chmod +w $(INSTALL_ROOT)/newbase
