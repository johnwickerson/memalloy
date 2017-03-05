pred hint[X:Exec] {
  /*
e1:RMW[rel](y,2,3) || e3:R[rlx](y,4) || e6:RMW[acq](x,1,2)
e2:W[rlx](y,4)     || e4:F[ar]
                   || e5:W[rlx](x,1)
============================================================
e7:RMW[acq](y,1,2) || e8:R[rlx](x,4)  || e11:RMW[rel](x,2,3) 
                   || e9:F[ar]        || e12:W[rlx](x,4)
                   || e10:W[rlx](y,1) ||
  */ 
  some disj e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12 : E {
    X.ev = e1+e2+e3+e4+e5+e6+e7+e8+e9+e10+e11+e12
	X.R = e1+e3+e6+e7+e8+e11
	X.W = e1+e2+e5+e6+e7+e10+e11+e12
	X.F = e4+e9
	X.A = X.ev
    X.rel = e1+e4+e9+e11
	X.acq = e4+e6+e7+e9
	X.sc = none
	X.ad = none->none
	X.dd = none->none
	X.cd = none->none
	X.sb = (e1->e2) + ^((e3->e4)+(e4->e5)) + ^((e8->e9)+(e9->e10)) + (e11->e12)
	X.sloc = sq[e5+e6+e8+e11+e12] + sq[e1+e2+e3+e7+e10]
    X.sthd = sq[e1+e2] + sq[e3+e4+e5] + sq[e6] + sq[e7] + sq[e8+e9+e10] + sq[e11+e12]
	X.rf = (e2->e3) + (e5->e6) + (e6->e11) + (e12->e8) + (e10->e7) + (e7->e1)
	X.co = ^((e1->e2) + (e5->e6) + (e6->e11) + (e11->e12) + (e10->e7) + (e7->e1))
  }
}
