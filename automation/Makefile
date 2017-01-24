all:
	ocamlbuild -cflag -annot -lib str cat2als.native
	mv cat2als.native cat2als

clean:
	ocamlbuild -clean
	rm -f cat2als
