install:
	opam install xml-light
	make -C src
	git submodule update --init --recursive
	make -C alloystar

quicktest: 
	@ tests/Q2_c11_lidbury_partial.sh

moretests:
	@ tests/Q2_sc_x86.sh
	@ tests/Q2_c11_lidbury_partial.sh
	@ tests/Q2_c11_sra_simp.sh
	@ tests/Q2_c11_simp_orig.sh
	@ tests/Q2_c11_simp_orig2.sh
	@ tests/Q2_sc_c11nodrf.sh
	@ tests/Q2_c11_lidbury_partial_iter.sh
	@ tests/Q2_c11_sra_simp_iter.sh

clean:
	python util/rm_als.py
	rm -f comparator.als
	make -C src clean
