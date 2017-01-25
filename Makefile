
install:
	make -C cat2als

models:
	cd hw; ../cat2als/cat2als -cn Exec_X86 -cp exec_x86 -o x86tso.als x86tso.cat

clean:
	make -C cat2als clean
	rm -f hw/x86tso.als
