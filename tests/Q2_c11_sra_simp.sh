comparator/comparator \
    -desc "Searching for an execution allowed by Batty et al's simplified C11 model but disallowed by Lahav et al.'s strong release/acquire model." \
    -events 6 \
    -relacq \
    -simplepost \
    -normws \
    -totalsb \
    models_als/c11_sra.als \
    models_als/c11_simp.als
