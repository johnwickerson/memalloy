pred iriw_shape[X:Exec] {
  some disj e0,e1,e2,e3,e4,e5 : E {
    X.EV = e0+e1+e2+e3+e4+e5
    X.W = e0+e3
    X.R = e1+e2+e4+e5
    X.sb = (e1 -> e2) + (e4 -> e5)
    X.sthd = sq[e0] + sq[e1+e2] + sq[e3] + sq[e4+e5]
    X.sloc = sq[e0+e1+e5] + sq[e2+e3+e4]
    X.stxn = sq[e0] + sq[e3]
  }
}
