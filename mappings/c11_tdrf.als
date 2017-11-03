open ../archs/exec_C[E]
open ../models/c11_txn[E] as M
open ../models/tsc[E] as N

module c11_tdrf[E]

pred p[X:Exec_C] {
  withoutinit[X]

  M/consistent[none->none, X]
  M/racefree[none->none, X]

  not (N/consistent[none->none,X])

  // all transactions are atomic
  no (X.stxn - X.atxn)

  // avoid non-SC atomic operations
  no (X.A - X.SC)

  X.EV = E
}

run p for 1 Exec, 7 E // started on benjamin 1448 on fri 3 nov
