./comparator -desc "adding_txns_ppc_4ev_allowset" -arch PPC -violates models/ppc_txn.cat -fencerels -events 4 -iter -expect 122 -allowset 
./comparator -desc "adding_txns_x86_4ev_allowset" -arch X86 -violates models/x86tso_txn.cat -fencerels -events 4 -iter -expect 42 -allowset # ~3 min
