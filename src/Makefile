# avoid a compiler warning
OCB_OPTS := -use-ocamlfind
# enable C-c C-t for inspecting types
OCB_OPTS := $(OCB_OPTS) -cflag -annot
# turns on debugging (use 'export OCAMLRUNPARAM=b' for stack traces)
OCB_OPTS := $(OCB_OPTS) -cflag -g
# standard OCaml libraries
OCB_OPTS := $(OCB_OPTS) -libs str,unix
# OCaml packages
OCB_OPTS := $(OCB_OPTS) -pkgs xml-light

# Set title of documentation
OCD_OPTS := -docflags -t,Memalloy
# Set custom stylesheet
OCD_OPTS := $(OCD_OPTS) -docflags -css-style,mystyle.css

.PHONY: all clean gen comparator doc

all: gen comparator # doc

gen: 
	ocamlbuild $(OCB_OPTS) gen.native
	mv gen.native ../gen

comparator:
	ocamlbuild $(OCB_OPTS) comparator.native
	mv comparator.native ../comparator

doc:
	python mk_odocl.py > comparator.odocl
	ocamlbuild $(OCB_OPTS) $(OCD_OPTS) comparator.docdir/index.html
	rm -rf ../doc && mv comparator.docdir ../doc
	cp mystyle.css ../doc/
	@ echo "HTML documentation is in ../doc/index.html."

clean:
	ocamlbuild -clean
	@ echo ""
	rm -f ../gen ../comparator
	rm -f comparator.odocl
	rm -rf ../doc
