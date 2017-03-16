./comparator \
    -desc "A miscompilation from C to Power" \
    -violates models/c11_simp.cat \
    -satisfies models/ppc.cat \
    -mapping mappings/c11_ppc.als \
    -arch C \
    -events 5 \
    -arch2 PPC \
    -events2 6 \
    -expect 1
