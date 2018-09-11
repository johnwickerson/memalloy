pred c11_strengthening_seq[X:Exec] {
/*
  e1:W[na](a,1) || e2:R[rlx](x,1) || e5: R[rlx](y,1)
                || [ctrl]         || [ctrl]
                || e3:R[na](a,1)  || e6: W[rlx](x,1)
                || [ctrl]         || 
                || e4:W[rlx](y,1) ||
*/
  some disj e1,e2,e3,e4,e5,e6 : E {
    X.EV = e1+e2+e3+e4+e5+e6
    X.R = e2+e3+e5
    X.W = e1+e4+e6
    X.F = none
    X.A = e2+e4+e5+e6
    X.REL = none
    X.ACQ = none
    X.SC = none
    X.ad = none->none
    X.dd = none->none
    X.cd = (e2->e3) + (e3->e4) + (e5->e6)
    X.sb = ^((e2->e3)+(e3->e4)) + (e5->e6)
    X.sloc = sq[e1+e3] + sq[e2+e6] + sq[e4+e5]
    X.sthd = sq[e1] + sq[e2+e3+e4] + sq[e5+e6]
    X.rf = (e1->e3) + (e6->e2) + (e4->e5)
    X.co = none->none
  }
}
