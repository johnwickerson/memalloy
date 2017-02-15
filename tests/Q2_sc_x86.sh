./comparator \
    -desc "Searching for executions allowed by x86 but disallowed by sequential consistency." \
    -arch X86 \
    -violates sc.cat \
    -satisfies x86tso.cat \
    -nodeps \
    -events 4 \
    -expect 2 \
    -iter \
    -minimal
