
install:
	make -C cat2als

ALSFILES = \
models_als/sc.als \
models_als/x86tso.als \
models_als/arm7.als \
models_als/ppc.als \
models_als/ptx_orig.als \
models_als/ptx_cumul.als

INTERMEDIATE_ALSFILES = \
models_als/basic.als \
models_als/ptx_base.als

all: $(INTERMEDIATE_ALSFILES) $(ALSFILES)

# Building .als files from corresponding .cat files
$(ALSFILES): models_als/%.als: models_cat/%.cat
	cd models_cat; ../cat2als/cat2als -o ../$@ ../$<

# Building .als files from corresponding .cat files
$(INTERMEDIATE_ALSFILES): models_als/%.als: models_cat/%.cat
	cd models_cat; ../cat2als/cat2als -i -o ../$@ ../$<

# Dependencies
models_als/x86tso.als: models_als/basic.als
models_als/parri.als: models_als/basic.als
models_als/arm7.als: models_als/basic.als
models_als/ppc.als: models_als/basic.als
models_als/sc.als: models_als/basic.als
models_als/ptx_base.als: models_als/basic.als
models_als/ptx_cumul.als: models_als/ptx_base.als
models_als/ptx_orig.als: models_als/ptx_base.als

clean:
	make -C cat2als clean
	rm -f $(INTERMEDIATE_ALSFILES)
	rm -f $(ALSFILES)
