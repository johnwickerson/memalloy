./comparator \
    -desc "Showing that version 2 of Lahav et al.'s PLDI'17 C11 model is stronger than version 1." \
    -violates models/c11_repairing2.cat \
    -satisfies models/c11_repairing1.cat \
    -arch C \
    -events 5 \
    -expect 1 \
    -satisfies models/c11_noscfences.cat \
    -satisfies models/c11_normws.cat \
    -satisfies models/totalsb.cat
