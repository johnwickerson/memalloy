./comparator \
    -desc "Comparing original C11 model against Batty et al's simplified model. No 4-event solutions expected." \
    -satisfies models/c11_orig.cat \
    -violates models/c11_simp.cat \
    -arch C \
    -events 4 \
    -expect 0
