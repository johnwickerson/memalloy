./comparator \
    -desc "Searching for executions allowed by x86 but disallowed by sequential consistency." \
    -arch X86 \
    -violates models/sc.cat \
    -satisfies models/x86tso.cat \
    -events 4 \
    -satisfies models/nodeps.cat \
    -expect 2 \
    -iter \
    -minimal
