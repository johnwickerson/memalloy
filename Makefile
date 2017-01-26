
install:
	make -C cat2als

# Building .als files from corresponding .cat files
models_als/%.als : models_cat/%.cat
	cd models_cat; \
	../cat2als/cat2als -o ../$@ ../$<

# Dependencies
models_als/x86tso.als: models_als/basic.als
models_als/parri.als: models_als/basic.als

clean:
	make -C cat2als clean
	rm -f models_als/x86tso.als
	rm -f models_als/basic.als
	rm -f models_als/parri.als
	rm -f models_als/shaked.als
	rm -f models_als/will.als
