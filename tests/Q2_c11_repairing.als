open ../sw/c11_repairing[E]

sig E {}

run {
  some X:Exec_C {
    not (consistent_batty[X])
    dead[X]
    consistent1[X]
    no_RMWs[X]
    no (X.F & X.sc)
  }
} for 1 Exec, 5 E expect 1

run {
  some X:Exec_C {
    not (consistent2[X])
    dead[X]
    consistent1[X]
    no_RMWs[X]
    no (X.F & X.sc)
  }
} for 1 Exec, 5 E expect 1

run {
  some X:Exec_C {
    not (consistent3[X])
    dead[X]
    consistent2[X]
    no_RMWs[X]
    //no (X.F & X.sc)
  }
} for 1 Exec, 7 E expect 1

run {
  some X:Exec_C {
    not (consistent3[X])
    dead[X]
    consistent4[X]
    no_RMWs[X]
    no (X.F & X.sc)
  }
} for 1 Exec, 6 E expect 1
