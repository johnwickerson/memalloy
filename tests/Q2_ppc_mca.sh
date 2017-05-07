./comparator \
    -desc "Searching for an execution that demonstrates that Power is not multi-copy atomic." \
    -arch PPC \
    -fencerels \
    -satisfies models/ppc.cat \
    -violates models/mca_ppc.cat \
    -events 6 \
    -expect 1 
