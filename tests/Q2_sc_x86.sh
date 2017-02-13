comparator/comparator \
    -desc "Searching for an execution allowed by x86 but disallowed by sequential consistency." \
    -arch X86 \
    -violates models_als/sc.als \
    -satisfies models_als/x86tso.als \
    -events 5 \
    -expect 2 \
    -iter \
    -nodeps \
    -minimal
