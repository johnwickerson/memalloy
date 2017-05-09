./comparator \
    -desc "Searching for all executions allowed by Batty et al's C11 model but disallowed by Lidbury et al.'s model." \
    -arch C \
    -violates models/c11_lidbury.cat \
    -satisfies models/c11_partial.cat \
    -events 4 \
    -expect 5 \
    -satisfies models/c11_normws.cat \
    -satisfies models/totalsb.cat \
    -iter \
    $@
