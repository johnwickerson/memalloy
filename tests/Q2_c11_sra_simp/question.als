open ../../sw/c11_sra[E] as M1
open ../../sw/c11_simp[E] as M2

sig E {}

pred gp [X : Exec_C] {
  
  // Prefer solutions without RMWs
  no_RMWs[X]  

  // Prefer solutions with total sb per thread
  total_sb[X]  

  // The execution is forbidden in SRA
  not(M1/consistent[X])
  M1/dead[X]

  // The execution is allowed (and not faulty) in the Simp model
  M2/consistent[X]

  // avoid executions where the postcondition must read shared locations
  X.co in (rc[X.rf]) . (rc[(X.sb) . (rc[~(X.rf)])])

  // stay within the release/acquire fragment
  X.R in X.acq
  X.W in X.rel
  no X.sc
}

pred hint2[X:Exec_C] {
  /*
    e1: W[rel](x,1)  || e4: W[rel](y,1)
    e2: W[rel](y,2)  || e5: W[rel](x,2)
    e3: R[acq](y,1)  || e6: R[acq](x,1)
  */ 
  some disj e1,e2,e3,e4,e5,e6 : E {
    X.ev = e1+e2+e3+e4+e5+e6
	X.R = e3+e6
	X.W = e1+e2+e4+e5
	X.F = none
    X.rel = X.W
	X.acq = X.R
	X.sc = none
	X.ad = none->none
	X.dd = none->none
	X.cd = none->none
	X.sb = ^((e1->e2)+(e2->e3)) + ^((e4->e5)+(e5->e6))
	X.sloc = sq[e1+e5+e6] + sq[e2+e3+e4]
    X.sthd = sq[e1+e2+e3] + sq[e4+e5+e6]
	X.rf = (e1->e6) + (e4->e3)
	X.co = (e2->e4) + (e5->e1)
  }
}

pred hint3[X:Exec_C] {
  /*
    e1: W[rel](x,1)  || e3: W[rel](y,1)
    e2: W[rel](y,2)  || e4: W[rel](x,2)
  */ 
  some disj e1,e2,e3,e4 : E {
    X.ev = e1+e2+e3+e4
	X.R = none
	X.W = e1+e2+e3+e4
	X.F = none
    X.rel = X.W
    X.acq = none
	X.sc = none
	X.ad = none->none
	X.dd = none->none
	X.cd = none->none
	X.sb = (e1->e2) + (e3->e4)
	X.sloc = sq[e1+e4] + sq[e2+e3]
    X.sthd = sq[e1+e2] + sq[e3+e4]
	X.rf = none->none
	X.co = (e2->e3) + (e4->e1)
  }
}

run gp for 1 Exec, 5 E, 3 Int expect 0 // <1s
run gp for 1 Exec, 6 E, 3 Int expect 1 // <1s

