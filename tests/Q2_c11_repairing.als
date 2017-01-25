open ../sw/c11_repairing[E]

sig E {}

run test1 {
  some X:Exec_C {
    not (consistent_batty[none,X])
    dead[none,X]
    consistent1[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 5 E expect 1

run test2 {
  some X:Exec_C {
    not (consistent2[none,X])
    dead[none,X]
    consistent1[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 5 E expect 1

run test3 {
  some X:Exec_C {
    not (consistent3[none,X])
    dead[none,X]
    consistent2[none,X]
    no_RMWs[none,X]
    //no (X.F & X.sc)
  }
} for 1 Exec, 7 E expect 1

run test4 {
  some X:Exec_C {
    not (consistent3[none,X])
    dead[none,X]
    consistent4[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 6 E expect 1

run test5 {
  some X:Exec_C {
    not (consistent5[none,X])
    dead[none,X]
    consistent4[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 6 E expect 1

run test6 {
  some X:Exec_C {
    not (consistent3[none,X])
    dead[none,X]
    consistent5[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 6 E expect 1
