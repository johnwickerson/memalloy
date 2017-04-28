ifndef BINARY
$(error "Can't call make here.")
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
# include lib directory
OCB_OPTS := $(OCB_OPTS) -I lib

# Set title of documentation
OCD_OPTS := -docflags -t,Memalloy
# Set custom stylesheet
OCD_OPTS := $(OCD_OPTS) -docflags -css-style,mystyle.css

# copy binary here
DEST := .

DOCDIR = doc

.PHONY: all clean

$(BINARY):
	@ echo "Building $(BINARY)..."
	@ ocamlbuild $(OCB_OPTS) $(BINARY).native
	@ mv $(BINARY).native $(DEST)/$(BINARY)
#	@ make doc

doc:
	@ python ../etc/mk_odocl.py > $(BINARY).odocl
	@ echo "Building documentation for $(BINARY)..."
	@ ocamlbuild $(OCB_OPTS) $(OCD_OPTS) $(BINARY).docdir/index.html
	@ rm -rf $(DOCDIR) && mv $(BINARY).docdir $(DOCDIR)
	@ cp ../etc/mystyle.css $(DOCDIR)
	@ echo "HTML docs are here: `pwd`/$(DOCDIR)/index.html."

clean:
	@ ocamlbuild -clean
	@ echo "" # because ocamlbuild doesn't end with a newline
	@ rm -f $(BINARY).odocl
	rm -f $(DEST)/$(BINARY)
