./comparator \
    -desc "Showing that Batty et al's POPL'16 C11 model is stronger than version 1 of Lahav et al.'s PLDI'17 model." \
    -violates models/c11_repairing0.cat \
    -satisfies models/c11_repairing1.cat \
    -arch C \
    -events 5 \
    -expect 1 \
    -satisfies models/c11_noscfences.cat \
    -satisfies models/c11_normws.cat \
    -satisfies models/totalsb.cat \
    $@
