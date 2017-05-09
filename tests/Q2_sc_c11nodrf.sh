./comparator \
    -desc "Finding a violation of the SC-DRF guarantee in an early draft of C11" \
    -satisfies models/c11_nodrf.cat \
    -violates models/sc.cat \
    -arch C \
    -events 4 \
    -satisfies models/c11_normws.cat \
    -satisfies models/c11_onlysc.cat \
    -satisfies models/totalsb.cat \
    -expect 1 \
    $@
