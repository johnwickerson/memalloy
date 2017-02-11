comparator/comparator \
    -desc "Searching for an execution allowed by Batty et al's C11 model but disallowed by Lidbury et al.'s model." \
    -arch C \
    -violates models_als/c11_lidbury.als \
    -satisfies models_als/c11_partial.als \
    -events 4 \
    -expect 1 \
    -normws \
    -totalsb
