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
# Complain about unused variables
UNUSED_VAR = @10@20@26@27@32..39
OCB_OPTS := $(OCB_OPTS) -cflags -w,$(UNUSED_VAR)

# Set title of documentation
DOCNAME := memalloy
OCD_OPTS := -docflags -t,$(DOCNAME)
# Set custom stylesheet
OCD_OPTS := $(OCD_OPTS) -docflags -css-style,mystyle.css

# copy binaries here
DEST := ..

DOCDIR := ../doc

.PHONY: all clean gen comparator doc

BINARIES = cat2als pp_comparator gen weaken

all: $(BINARIES) doc 

$(BINARIES): 
	@ ocamlbuild $(OCB_OPTS) $@.native
	mv $@.native $(DEST)/$@

doc:
	@ python ../etc/mk_odocl.py > $(DOCNAME).odocl
	@ ocamlbuild $(OCB_OPTS) $(OCD_OPTS) $(DOCNAME).docdir/index.html
	@ rm -rf $(DOCDIR) && mv $(DOCNAME).docdir $(DOCDIR)
	@ cp mystyle.css $(DOCDIR)
	@ echo "HTML documentation is in $(DOCDIR)/index.html."

clean:
	ocamlbuild -clean
	@ echo "" # because ocamlbuild doesn't end with a newline
	rm -f $(DEST)/gen
	rm -f $(DEST)/cat2als
	rm -f $(DEST)/pp_comparator
	rm -f $(DEST)/weaken
	rm -f memalloy.odocl
	rm -rf $(DOCDIR)
