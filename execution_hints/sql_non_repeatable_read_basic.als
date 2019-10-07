pred sql_non_repeatable_read_basic[X:Exec_SQL] {
  /*
  RR  || *   || *
  ====++=====++=====
  Rx1 || Wx1 || Wx2
  Rx2 ||     ||
  C   || C   || C
  */
  some disj R1, R2, W1, W2, C1, C2, C3 : E {
    R1+R2 in X.R
    W1+W2 in X.W
    C1+C2+C3 in X.C

    (R1->R2) + (R2->C1) in X.sthd
    (W1->C2) in X.sthd
    (W2->C3) in X.sthd

    (C1->C2) not in X.sthd
    (C2->C3) not in X.sthd

    (R1->R2) in X.sb
    (W1->R1) in X.rf
    (W2->R2) in X.rf

    (R1+R2+C1) in X.RR
    (W1+C2) in X.RU
    (W2+C3) in X.RU
  }
}
