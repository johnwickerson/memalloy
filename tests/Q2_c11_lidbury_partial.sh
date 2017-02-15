./comparator \
    -desc "Searching for an execution allowed by Batty et al's C11 model but disallowed by Lidbury et al.'s model." \
    -arch C \
    -violates c11_lidbury.cat \
    -satisfies c11_normws.cat \
    -events 4 \
    -expect 1 \
    -satisfies c11_partial.cat \
    -totalsb
