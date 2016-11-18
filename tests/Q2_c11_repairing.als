open ../sw/c11_repairing[E]

sig E {}

run test1 {
  some X:Exec_C {
    not (consistent_batty[X])
    dead[X]
    consistent1[X]
    no_RMWs[X]
    no (X.F & X.sc)
  }
} for 1 Exec, 5 E expect 1

run test2 {
  some X:Exec_C {
    not (consistent2[X])
    dead[X]
    consistent1[X]
    no_RMWs[X]
    no (X.F & X.sc)
  }
} for 1 Exec, 5 E expect 1

run test3 {
  some X:Exec_C {
    not (consistent3[X])
    dead[X]
    consistent2[X]
    no_RMWs[X]
    //no (X.F & X.sc)
  }
} for 1 Exec, 7 E expect 1

run test4 {
  some X:Exec_C {
    not (consistent3[X])
    dead[X]
    consistent4[X]
    no_RMWs[X]
    no (X.F & X.sc)
  }
} for 1 Exec, 6 E expect 1

run test5 {
  some X:Exec_C {
    not (consistent5[X])
    dead[X]
    consistent4[X]
    no_RMWs[X]
    no (X.F & X.sc)
  }
} for 1 Exec, 6 E expect 1

run test6 {
  some X:Exec_C {
    not (consistent3[X])
    dead[X]
    consistent5[X]
    no_RMWs[X]
    no (X.F & X.sc)
  }
} for 1 Exec, 6 E expect 1
