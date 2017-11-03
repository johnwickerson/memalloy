open ../archs/exec_C[E]
open ../models/c11_txn[E] as M

module c11_strong_isolation[E]

pred p[X:Exec_C] {
  withoutinit[X]

  M/consistent[none->none, X]
  M/racefree[none->none, X]

  let com = X.rf + X.co + fr[none->none,X] |
 // not (is_acyclic[weaklift[com, X.stxn, none->none, X]])
  not (is_acyclic[stronglift[com, X.atxn, none->none, X]])

  X.EV = E
}

run p for 1 Exec, 6 E
