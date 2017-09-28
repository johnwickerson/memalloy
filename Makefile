.PHONY: build install quicktest moretests slowtests clean deepclean

rebuild:
	make -C src
	@ make comparator
	@ make -C mappings
	@ make -C archs

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
	ARGS="-batch" make $(TXN_TESTS)
	ARGS="-batch -maxtransactions 0" make $(ALL_TESTS)

slowtests:

clean:
	cd models && python ../etc/rm_als.py
	make -C src clean
	make -C mappings clean
	make -C archs clean
	rm -f top/*.pyc
	rm -f comparator

deepclean:
	make -C alloystar clean
	make clean
