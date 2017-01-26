
install:
	make -C cat2als

models_als/basic.als:
	cd models_cat; \
	../cat2als/cat2als -o ../models_als/basic.als basic.cat

models_als/x86tso.als: models_als/basic.als
	cd models_cat; \
	../cat2als/cat2als -o ../models_als/x86tso.als x86tso.cat

clean:
	make -C cat2als clean
	rm -f models_als/x86tso.als
	rm -f models_als/basic.als
