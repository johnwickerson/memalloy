./comparator \
    -desc "A miscompilation from C to Arm7" \
    -violates models/c11_simp.cat \
    -satisfies models/arm7.cat \
    -mapping mappings/c11_arm7.als \
    -arch C \
    -arch2 ARM7 \
    -events2 6 \
    -events 6 \
    -alsosatisfies models/c11_normws.cat \
    -alsosatisfies models/nofences.cat \
    -minimal \
    -expect 1

# Also possible:
#   -events 5 -events2 6
# if the "no rmws" restriction is removed
