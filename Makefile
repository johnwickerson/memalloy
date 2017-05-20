.PHONY: build install quicktest moretests slowtests clean deepclean

rebuild:
	make -C src
	@ make comparator
	@ make -C mappings

install:
	git submodule update --init --recursive
	make -C alloystar
	make rebuild

comparator:
	@ echo "Building top-level executable."
	@ rm -f $@
	@ printf '#!/bin/sh\npython top/comparator.py "$$@"' > $@
	@ chmod +x $@

include tests.makefile

quicktest: 
	make c11_partial_not_lidbury

moretests:
	ARGS="-batch" make $(ALL_TESTS)

slowtests:

clean:
	python etc/rm_als.py
	make -C src clean
	make -C mappings clean
	rm -f top/*.pyc
	rm -f comparator

deepclean:
	make -C alloystar clean
	make clean
