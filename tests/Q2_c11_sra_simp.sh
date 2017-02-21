./comparator \
    -desc "An execution allowed by Batty et al's simplified C11 model but disallowed by Lahav et al.'s strong release/acquire model." \
    -violates models/c11_sra.cat \
    -satisfies models/c11_simp.cat \
    -arch C \
    -events 6 \
    -expect 1 \
    -satisfies models/c11_relacq.cat \
    -satisfies models/c11_normws.cat \
    -satisfies models/c11_simplepost.cat \
    -satisfies models/totalsb.cat
 #   -hint execution_hints/c11_sra_simp.als
