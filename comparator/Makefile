# avoid a compiler warning
OCB_OPTS := -use-ocamlfind
# enable C-c C-t for inspecting types
OCB_OPTS := $(OCB_OPTS) -cflag -annot
# standard OCaml libraries
OCB_OPTS := $(OCB_OPTS) -libs str,unix
# OCaml packages
OCB_OPTS := $(OCB_OPTS) -pkgs xml-light

MAINS = gen cat2als comparator

.PHONY: all clean

all: $(MAINS)
	python mk_odocl.py > comparator.odocl
	ocamlbuild $(OCB_OPTS) comparator.docdir/index.html
	mv comparator.docdir doc
	@ echo "HTML documentation is in doc/index.html."

$(MAINS):
	ocamlbuild $(OCB_OPTS) $@.native
	mv $@.native $@

clean:
	ocamlbuild -clean
	rm -f $(MAINS)
	rm -f comparator.odocl
	rm -rf doc
