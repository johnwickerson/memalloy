python top/comparator.py \
    -desc "Searching for an execution allowed by Batty et al's C11 model but disallowed by Lidbury et al.'s model." \
    -arch C \
    -violates models/c11_lidbury.cat \
    -satisfies models/c11_partial.cat \
    -events 4 \
    -expect 1 \
    -satisfies models/c11_normws.cat \
    -satisfies models/totalsb.cat
