./comparator \
    -desc "Comparing original C11 model against Batty et al's simplified model" \
    -satisfies models/c11_orig.cat \
    -violates models/c11_simp.cat \
    -arch C \
    -events 5 \
    -expect 1 \
    $@
