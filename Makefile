build:
	jbuilder build

test:
	jbuilder runtest

examples:
	jbuilder build @examples

doc:
	jbuilder build @doc

clean:
	jbuilder clean

all-supported-ocaml-versions:
	jbuilder build @install @runtest --workspace jbuild-workspace.dev

.PHONY: build test doc clean examples all-supported-ocaml-versions
