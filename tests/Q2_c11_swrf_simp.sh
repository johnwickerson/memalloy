./comparator \
    -desc "An execution allowed by Batty et al's simplified C11 model but disallowed by Nienhuis et al.'s extra axiom." \
    -violates models/c11_swrf.cat \
    -satisfies models/c11_simp.cat \
    -arch C \
    -events 12 \
    -expect 1 \
    -solver plingeling \
    -satisfies models/totalsb.cat \
    -threads 6 \
    -locations 2
    #-satisfies models/c11_nosc.cat
    # -hint execution_hints/nienhuis_example.als
