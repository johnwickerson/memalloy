# avoid a compiler warning
OCB_OPTS := -use-ocamlfind
# enable C-c C-t for inspecting types
OCB_OPTS := $(OCB_OPTS) -cflag -annot
# standard OCaml libraries
OCB_OPTS := $(OCB_OPTS) -libs str,unix
# OCaml packages
OCB_OPTS := $(OCB_OPTS) -pkgs xml-light

# Set title of documentation
OCD_OPTS := -docflags -t,Comparator
# Set custom stylesheet
OCD_OPTS := $(OCD_OPTS) -docflags -css-style,mystyle.css

MAINS = gen cat2als comparator

.PHONY: all clean

all: $(MAINS)
	python mk_odocl.py > comparator.odocl
	ocamlbuild $(OCB_OPTS) $(OCD_OPTS) comparator.docdir/index.html
	rm -rf ../doc && mv comparator.docdir ../doc
	cp mystyle.css ../doc/
	@ echo "HTML documentation is in ../doc/index.html."

$(MAINS):
	ocamlbuild $(OCB_OPTS) $@.native
	mv $@.native $@

clean:
	ocamlbuild -clean
	rm -f $(MAINS)
	rm -f comparator.odocl
	rm -rf ../doc
