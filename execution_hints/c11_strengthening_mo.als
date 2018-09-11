pred c11_strengthening_mo[X:Exec] {
/*
  	e1:W[na](a,1)   ||  e5: R[acq](x,1)
	e2:F[rlx]       ||  e6: R[na](a,1)
	e3:R[rlx](y,1)  ||  e7: W[rlx](y,1)
	e4:W[rlx](x,1)  ||
*/
  some disj e1,e2,e3,e4,e5,e6,e7 : E {
    X.EV = e1+e2+e3+e4+e5+e6+e7
    X.R = e3+e5+e6
    X.W = e1+e4+e7
    X.F = e2
    X.A = e2+e3+e4+e5+e7
    X.REL = none
    X.ACQ = e5
    X.SC = none
    X.ad = none->none
    X.dd = none->none
    X.cd = (e3->e4) + (e5->e6) + (e6->e7)
    X.sb = ^((e1->e2)+(e2->e3)+(e3->e4)) + ^((e5->e6)+(e6->e7))
    X.sloc = sq[e1+e6] + sq[e3+e7] + sq[e4+e5]
    X.sthd = sq[e1+e2+e3+e4] + sq[e5+e6+e7]
    X.rf = (e1->e6) + (e4->e5) + (e7->e3)
    X.co = none->none
  }
}
