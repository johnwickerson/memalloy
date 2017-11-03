open ../archs/exec_C[E]
open ../models/c11_txn[E] as M
open ../models/tsc[E] as N

module c11_tdrf[E]

pred p[X:Exec_C] {
  withoutinit[X]

  M/consistent[none->none, X]
  M/racefree[none->none, X]

  not (N/consistent[none->none,X])

  X.EV = E
}

run p for 1 Exec, 6 E
