# avoid a compiler warning
OCB_OPTS := -use-ocamlfind
# enable C-c C-t for inspecting types
OCB_OPTS := $(OCB_OPTS) -cflag -annot
# turns on recursive building, in case there are modules in
# subdirectories. The main reason is to disable an annoying
# warning about not having it on!
OCB_OPTS := $(OCB_OPTS) -r
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

.PHONY: all clean doc top cat2als pp_comparator gen weaken mk_hash 

BINARIES = cat2als pp_comparator gen weaken mk_hash comparator

all: $(BINARIES) doc 

$(BINARIES): 
	@ ocamlbuild $(OCB_OPTS) $@.native
	mv $@.native $(DEST)/$@

# Note: put `#directory "_build";;` into your ~/.ocamlinit file
top:
	@ python ../etc/list_all_module_names.py > toplevel.mltop
	@ ocamlbuild toplevel.top $(OCB_OPTS)
	rlwrap ./toplevel.top

doc:
	@ python ../etc/list_all_module_names.py > $(DOCNAME).odocl
	@ ocamlbuild $(OCB_OPTS) $(OCD_OPTS) $(DOCNAME).docdir/index.html
	@ rm -rf $(DOCDIR) && mv $(DOCNAME).docdir $(DOCDIR)
	@ cp mystyle.css $(DOCDIR)
	@ echo "HTML documentation is in $(DOCDIR)/index.html."

clean:
	ocamlbuild -clean
	@ echo "" # because ocamlbuild doesn't end with a newline
	cd $(DEST); rm -f $(BINARIES)
	rm -f memalloy.odocl
	rm -f toplevel.mltop
	rm -rf $(DOCDIR)
