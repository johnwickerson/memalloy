
install:
	make -C cat2als

ALSFILES = \
models_als/basic.als \
models_als/sc.als \
models_als/x86tso.als \
models_als/arm7.als \
models_als/ppc.als

all:
	make $(ALSFILES)

# Building .als files from corresponding .cat files
models_als/%.als : models_cat/%.cat
	cd models_cat; \
	../cat2als/cat2als -o ../$@ ../$<

# Dependencies
models_als/x86tso.als: models_als/basic.als
models_als/parri.als: models_als/basic.als
models_als/arm7.als: models_als/basic.als
models_als/ppc.als: models_als/basic.als
models_als/sc.als: models_als/basic.als

clean:
	make -C cat2als clean
	rm -f $(ALSFILES)
