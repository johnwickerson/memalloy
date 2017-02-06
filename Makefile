
install:
	opam install xml-light.2.4
	make -C cat2als
	make -C gen
	make -C comparator
	git submodule update --init --recursive
	make -C alloystar

ALSFILES = \
models_als/sc.als \
models_als/x86tso.als \
models_als/arm7.als \
models_als/ppc.als \
models_als/ptx_orig.als \
models_als/ptx_cumul.als \
models_als/aarch64.als \
models_als/c11_simp.als \
models_als/c11_partial.als \
models_als/c11_sra.als \
models_als/c11_swrf.als \
models_als/c11_lidbury.als \
models_als/c11_repairing0.als \
models_als/c11_repairing1.als \
models_als/c11_repairing2.als \
models_als/c11_repairing3.als \
models_als/c11_repairing4.als \
models_als/c11_repairing5.als \
models_als/opencl_orig.als \
models_als/opencl_simp.als \
models_als/opencl_scoped.als \

INTERMEDIATE_ALSFILES = \
models_als/basic.als \
models_als/basic_H.als \
models_als/ptx_base.als \
models_als/c11_base.als \
models_als/opencl_base.als

models: $(INTERMEDIATE_ALSFILES) $(ALSFILES)

quicktest: c11_sra_simp

c11_sra_simp: $(INTERMEDIATE_ALSFILES) $(ALSFILES)
	cd comparator; \
	./comparator -relacq -simplepost -normws -totalsb \
	-events 6 \
	../models_als/c11_sra.als \
	../models_als/c11_simp.als > comparator.als; \
	./do_compare.sh comparator.als

# Building .als files from corresponding .cat files
$(ALSFILES): models_als/%.als: models_cat/%.cat
	cd models_cat; ../cat2als/cat2als ../$<

# Building .als files from corresponding .cat files
$(INTERMEDIATE_ALSFILES): models_als/%.als: models_cat/%.cat
	cd models_cat; ../cat2als/cat2als -i ../$<

clean:
	make -C cat2als clean
	rm -f $(INTERMEDIATE_ALSFILES)
	rm -f $(ALSFILES)
	make -C gen clean
	make -C comparator clean
	make -C alloystar clean
