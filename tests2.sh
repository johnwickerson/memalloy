./comparator -desc "weak_vs_strong_atomicity_3ev" -arch HW -satisfies models/txn_weak_atomicity.cat -satisfies models/sc.cat -violates models/txn_strong_atomicity.cat -events 3 -iter -expect 4
./comparator -desc "weak_vs_strong_atomicity_5ev" -arch HW -satisfies models/txn_weak_atomicity.cat -satisfies models/sc.cat -violates models/txn_strong_atomicity.cat -events 5 -exact -iter -expect 45 # ~30 secs
./comparator -desc "adding_txns_arm8_3ev" -arch ARM8 -satisfies models/aarch64.cat -violates models/aarch64_txn.cat -fencerels -events 3 -iter -expect 10 # ~6 secs
./comparator -desc "adding_txns_arm8_4ev" -arch ARM8 -satisfies models/aarch64.cat -violates models/aarch64_txn.cat -fencerels -events 4 -iter -expect 139 # ~2 min
./comparator -desc "adding_txns_arm8_4ev_allowset" -arch ARM8 -violates models/aarch64_txn.cat -fencerels -events 4 -iter -expect 196 -allowset
