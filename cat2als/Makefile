all:
	@ echo "Attention: requires OCaml version >= 4.01.0."
	ocamlbuild -cflag -annot -lib str -lib unix cat2als.native
	mv cat2als.native cat2als

clean:
	ocamlbuild -clean
	rm -f cat2als
