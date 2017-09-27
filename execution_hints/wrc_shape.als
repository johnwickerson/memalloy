pred wrc_shape[X:Exec] {
  some disj e0,e1,e2,e3,e4 : E {
    X.EV = e0+e1+e2+e3+e4
    X.sb = (e1 -> e2) + (e3 -> e4)
    X.sthd = sq[e0] + sq[e1+e2] + sq[e3+e4]
    X.sloc = sq[e0+e1+e4] + sq[e2+e3]
    X.stxn = sq[e0]
  }
}
