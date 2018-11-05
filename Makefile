build:
	dune build

test:
	dune runtest

examples:
	dune build @examples

doc:
	dune build @doc

clean:
	dune clean

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev

.PHONY: build test doc clean examples all-supported-ocaml-versions
