./comparator \
    -desc "Searching for a miscompilation from OCaml to Power" \
    -violates models/ocaml.cat \
    -alsosatisfies models/ocaml_restrictions.cat \
    -satisfies models/ppc.cat \
    -mapping mappings/ocaml_ppc_v2.als \
    -arch OCaml \
    -events 5 \
    -arch2 PPC \
    -events2 5 \
    -minimal \
    -expect 0 # about 4 mins
