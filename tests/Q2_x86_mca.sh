./comparator \
    -desc "Searching (in vain) for an execution that demonstrates that x86 is not multi-copy atomic." \
    -arch X86 \
    -satisfies models/x86tso.cat \
    -violates models/mca_x86.cat \
    -events 7 \
    -expect 0
