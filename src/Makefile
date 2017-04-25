ifndef MEMALLOY_ROOT_DIR
$(error MEMALLOY_ROOT_DIR is undefined)
endif

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

# copy binaries here
DEST := $(MEMALLOY_ROOT_DIR)/top

.PHONY: all clean gen comparator doc

all: gen cat2als pp_comparator doc 

gen: 
	ocamlbuild $(OCB_OPTS) $@.native
	mv $@.native $(DEST)/$@

cat2als:
	ocamlbuild $(OCB_OPTS) $@.native
	mv $@.native $(DEST)/$@

pp_comparator:
	ocamlbuild $(OCB_OPTS) $@.native
	mv $@.native $(DEST)/$@

doc:
	python mk_odocl.py > comparator.odocl
	ocamlbuild $(OCB_OPTS) $(OCD_OPTS) comparator.docdir/index.html
	rm -rf ../doc && mv comparator.docdir ../doc
	cp mystyle.css ../doc/
	@ echo "HTML documentation is in ../doc/index.html."

clean:
	ocamlbuild -clean
	@ echo ""
	rm -f $(DEST)/gen
	rm -f $(DEST)/cat2als
	rm -f $(DEST)/pp_comparator
	rm -f comparator.odocl
	rm -rf ../doc
