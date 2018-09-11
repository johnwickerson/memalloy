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

tests:
	./runtests tests.sh

slowtests:
	./runtests -withslow tests.sh

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
