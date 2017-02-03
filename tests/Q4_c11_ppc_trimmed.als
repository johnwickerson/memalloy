open ../mappings/c11_ppc_trimmed[SE, HE]
open ../models_als/c11_partial[SE] as M1
open ../models_als/ppc[HE] as M2

sig SE,HE {}

pred gp [
  X : Exec_C, X' : Exec_PPC, 
  map: SE -> HE
] {
  
  withoutinit[X]
  withoutinit[X']
  
  no_RMWs[none,X]

  // we have a valid application of the mapping
  apply_map[X, X', map]

  // The execution is forbidden in software ...
  not(M1/consistent[none,X])
  M1/dead[none,X]
      
  // ... but can nonetheless be observed on the hardware.
  M2/consistent[none,X']
    
}

/*
The following hint finds a C11/Power bug when we use the
c11_simp model proposed by Batty et al (POPL16).
*/
pred hint1[X:Exec_C] {

  // e1: Wsc x 1   e3: Rsc y 1   e4: Wsc y 2
  // e2: Wrel y 1                e5: Rsc x 0

  some disj e1,e2,e3,e4,e5 : SE {
    X.ev = e1+e2+e3+e4+e5
    X.R = e3+e5
    X.W = e1+e2+e4
    X.F = none
    X.A = X.ev
    X.rel = e1+e2+e4
    X.acq = e3+e5
    X.sc = e1+e3+e4+e5
    X.sb = (e1->e2) + (e4->e5)
    X.sloc = sq[e1+e5] + sq[e2+e3+e4]
    X.sthd = sq[e1+e2] + sq[e3] + sq[e4+e5]
    X.cd = none->none
    X.ad = none->none
    X.dd = none->none
    X.rf = (e2 -> e3)
    X.co = (e2 -> e4)
  }
}

// The following hint finds a C11/Power bug when we use the
// c11_partial model (which avoids Batty et al.'s simplifications
// to the SC semantics). It was discovered by Ori Lahav et al.
pred hint2_sw[X:Exec_C] {
  // e1:W[rlx]x=2    e4:W[sc]y=1    e7:R[sc]x=1
  // e2:F[sc]        e5:W[rel]x=1
  // e3:R[rlx]y=0    e6:R[rlx]x=2
  some disj e1,e2,e3,e4,e5,e6,e7 : SE {
    X.ev = e1+e2+e3+e4+e5+e6+e7
    X.R = e3+e6+e7
    X.W = e1+e4+e5
    X.F = e2
    X.A = X.ev
    X.rel = e2+e4+e5
    X.acq = e2+e7
    X.sc = e2+e4+e7
	X.sb = ^((e1->e2)+(e2->e3)) + ^((e4->e5)+(e5->e6))
    X.sloc = sq[e1+e5+e6+e7] + sq[e3+e4]
    X.sthd = sq[e1+e2+e3] + sq[e4+e5+e6] + sq[e7]
    X.cd = none->none
	X.ad = none->none
	X.dd = none->none
	X.rf = (e1->e6) + (e5->e7)
	X.co = (e5->e1)
  }
}

pred hint2_hw[X:Exec_PPC] {
  // e1:W x=2    e4:W y=1    e8:R x=1
  // e2:sync     e5:lwsync
  // e3:R y=0    e6:W x=1
  //             e7:R x=2
  some disj e1,e2,e3,e4,e5,e6,e7,e8 : HE {
    X.ev = e1+e2+e3+e4+e5+e6+e7+e8
    X.R = e3+e7+e8
    X.W = e1+e4+e6
    X.sync = e2
    X.lwsync = e5
    X.F = X.sync+X.lwsync
	X.sb = ^((e1->e2) + (e2->e3)) + 
           ^((e4->e5) + (e5->e6) + (e6->e7))
    X.sloc = sq[e1+e6+e7+e8] + sq[e3+e4]
    X.sthd = sq[e1+e2+e3] + sq[e4+e5+e6+e7] + sq[e8]
    X.cd = none->none
	X.ad = none->none
	X.dd = none->none
	X.rf = (e1->e7) + (e6->e8)
	X.co = (e6->e1)
  }
}

run gp for
exactly 1 c11_ppc_trimmed/SW/exec/Exec,
exactly 1 c11_ppc_trimmed/HW/exec_H/exec/Exec,
7 HE, 
6 SE
expect 1
// Glucose: about 2 min. This finds essentially the same example
// as is given by Lahav et al. in Appendix A.1 of
// http://plv.mpi-sws.org/scfix/paper.pdf#page=15
