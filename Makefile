include $(shell ocamlc -where)/Makefile.config

OCAMLBUILD=ocamlbuild -j 0 -use-ocamlfind -classic-display \
	-plugin-tag 'package(cppo_ocamlbuild)'

build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true

test: build
	rm -rf _build/src_test/
	$(OCAMLBUILD) src_test/test_ppx_deriving.byte --

examples: build
	rm -rf _build/src_examples/
	$(OCAMLBUILD) src_examples/print_test.byte

doc:
	$(OCAMLBUILD) doc/api.docdir/index.html \
						 -docflags -t -docflag "API reference for ppx_deriving" \
						 -docflags '-colorize-code -short-functors -charset utf-8' \
						 -docflags '-css-style style.css'
	cp doc/style.css api.docdir/

clean:
	ocamlbuild -clean

.PHONY: build test doc clean examples

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp -t .gh-pages/ api.docdir/*
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
	opam publish prepare $(NAME_VERSION) $(ARCHIVE)
	cp -t $(NAME_VERSION) descr
	grep -Ev '^(name|version):' opam >$(NAME_VERSION)/opam
	opam publish submit $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

install:
	ocamlfind remove ppx_deriving
	grep -E '^[[:space:]]+' ppx_deriving.install | \
		awk '{ print $$1 }' | \
		sed -e 's:"?*::g'  | \
		xargs ocamlfind install ppx_deriving
	mv `ocamlfind query ppx_deriving -suffix /ppx_deriving_main.native` `ocamlfind query ppx_deriving -suffix /ppx_deriving$(EXE)`

.PHONY: gh-pages release
