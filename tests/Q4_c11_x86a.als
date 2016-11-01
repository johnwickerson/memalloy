open ../mappings/c11_x86a[SE,HE]
open ../sw/c11_simp[SE] as M1
open ../hw/x86tso[HE] as M2

sig SE, HE {}

pred gp [X : Exec_C, X' : Exec_X86, map: SE -> HE] {
        
  // we have a valid application of the mapping
  apply_map[X, X', map]

  // The execution is forbidden in software ...
  not(M1/consistent[X])
  M1/dead[X]

  // ... but can nonetheless be observed on the hardware.
  M2/consistent[X']

  //hint_sw[X]
  //hint_hw[X']
 
  // simplifying the solution (optional)
  no_RMWs[X]

}

pred hint_sw[X:Exec_C] {
  some disj e1, e2, e3, e4 : SE {
    X.ev = e1+e2+e3+e4
	X.R = e1+e2+e3+e4
	X.failedCAS = e2+e4
	X.W = e1+e3
	X.F = none
	X.sc = e1+e2+e3+e4
    X.sb = (e1->e2) + (e3->e4)
    X.ad = none->none
	X.dd = none->none
	X.cd = none->none
	X.sthd = sq[e1+e2] + sq[e3+e4]
	X.sloc = sq[e1+e4] + sq[e2+e3]
	X.rf = none->none
	X.co = none->none
  }
}

pred hint_hw[X:Exec_X86] {
  some disj e1,e2,e3,e4,e5,e6 : HE {
    X.ev = e1+e2+e3+e4+e5+e6
	X.R = e1+e3+e4+e6
	X.W = e2+e5
	X.locked = X.ev
	X.atom = (e1->e2) + (e4->e5)
	X.sb = ^((e1->e2) + (e2->e3)) + ^((e4->e5) + (e5->e6))
	X.sthd = sq[e1+e2+e3] + sq[e4+e5+e6]
	X.sloc = sq[e1+e2+e6] + sq[e4+e5+e3]
	X.rf = none->none
	X.co = none->none
  }
}

run gp for
exactly 1 c11_x86a/SW/exec/Exec,
exactly 1 c11_x86a/HW/exec_H/exec/Exec,
6 HE, 
4 SE

/*
This finds a bug in the x86tso model.
<1s, PLingeling, Babillion
*/
