pred sql_non_repeatable_read_initial[X:Exec_SQL] {
  /*
  RR  || *
  ====++=====
  Rx0 || Wx1
  Rx1 ||
  C   || C
  */
  some disj R1, R2, W1, C1, C2 : E {
    R1+R2 in X.R
    W1 in X.W
    C1+C2 in X.C

    (R1->R2) + (R2->C1) in X.sthd
    (W1->C2) in X.sthd

    (C1->C2) not in X.sthd

    (R1->R2) in X.sb

    no X.rf :> R1
    (W1->R2) in X.rf

    (R1+R2+C1) in X.RR
    (W1+C2) in X.RU
  }
}
