./comparator \
    -desc "Showing that version 3 of Lahav et al.'s PLDI'17 C11 model is stronger than version 2." \
    -violates models/c11_repairing3.cat \
    -satisfies models/c11_repairing2.cat \
    -arch C \
    -events 7 \
    -expect 1 \
    -satisfies models/c11_normws.cat \
    -satisfies models/totalsb.cat \
    $@
