./comparator \
    -desc "Showing that version 3 of Lahav et al.'s PLDI'17 C11 model is stronger than version 4." \
    -violates models/c11_repairing3.cat \
    -satisfies models/c11_repairing4.cat \
    -arch C \
    -events 6 \
    -expect 1 \
    -satisfies models/c11_noscfences.cat \
    -satisfies models/c11_normws.cat \
    -satisfies models/totalsb.cat
