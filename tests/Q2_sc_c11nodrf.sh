./comparator \
    -desc "Finding a violation of the SC-DRF guarantee in an early draft of C11" \
    -satisfies c11_nodrf.cat \
    -violates sc.cat \
    -arch C \
    -events 4 \
    -satisfies c11_normws.cat \
    -satisfies c11_onlysc.cat \
    -expect 1
