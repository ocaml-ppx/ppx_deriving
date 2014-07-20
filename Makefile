build:
	ocaml pkg/build.ml native=true native-dynlink=true

test: build
	rm _build/src_test/ -rf
	ocamlbuild -use-ocamlfind -package oUnit -I src \
  					 src_test/test_ppx_deriving.byte --

clean:
	ocamlbuild -clean

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

release:
	make do-release VERSION=$(oasis query Version)

do-release:
	git checkout -B release
	oasis setup
	git add .
	git commit -m "Generate OASIS files."
	git tag -a v$(VERSION) -m "Version $(VERSION)"
	git checkout @{-1}
	git branch -D release
	git push origin v$(VERSION)

.PHONY: gh-pages release
