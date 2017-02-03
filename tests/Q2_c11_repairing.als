open ../models_als/c11_repairing0[E] as M0
open ../models_als/c11_repairing1[E] as M1
open ../models_als/c11_repairing2[E] as M2
open ../models_als/c11_repairing3[E] as M3
open ../models_als/c11_repairing4[E] as M4
open ../models_als/c11_repairing5[E] as M5


sig E {}

run test1 {
  some X:Exec_C {
    withoutinit[X]
    not (M0/consistent[none,X])
    M0/dead[none,X]
    M1/consistent[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 5 E expect 1

run test2 {
  some X:Exec_C {
    withoutinit[X]
    not (M2/consistent[none,X])
    M2/dead[none,X]
    M1/consistent[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 5 E expect 1

run test3 {
  some X:Exec_C {
    withoutinit[X]
    not (M3/consistent[none,X])
    M3/dead[none,X]
    M2/consistent[none,X]
    no_RMWs[none,X]
    //no (X.F & X.sc)
  }
} for 1 Exec, 7 E expect 1

run test4 {
  some X:Exec_C {
    withoutinit[X]
    not (M3/consistent[none,X])
    M3/dead[none,X]
    M4/consistent[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 6 E expect 1

run test5 {
  some X:Exec_C {
    withoutinit[X]
    not (M5/consistent[none,X])
    M5/dead[none,X]
    M4/consistent[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 6 E expect 1

run test6 {
  some X:Exec_C {
    withoutinit[X]
    not (M3/consistent[none,X])
    M3/dead[none,X]
    M5/consistent[none,X]
    no_RMWs[none,X]
    no (F[none,X] & sc[none,X])
  }
} for 1 Exec, 6 E expect 1
