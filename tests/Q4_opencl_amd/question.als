open ../../sw/opencl/opencl_scopedsc[E] as M1
open ../../hw/gpu/amd_gpu as M2
open ../../mappings/opencl_amd[E]

sig E {}

pred gp [X : M1/Exec_OpenCL, map: E -> M2/Inst] {
  // we have a valid application of the mapping
  apply_map[X, map]

  // Sequenced-before is total within each thread
  total_sb[X]

  // the AMD model doesn't include local memory
  no X.L

  // The execution is not consistent in software
  not (M1/consistent[X])
  
  // Execution is dead
  M1/dead[X]

  // The execution is observable on hardware
  M2/consistent

  //mp_hint[X] 
  //mp_forced

  //atom_hint[X]
  //atom_forced_2
}

pred mp_hint [X:M1/Exec_OpenCL] {

  some disj e1, e2, e3, e4 : E {
    X.ev = e1 + e2 + e3 + e4
    X.R = e3 + e4
    X.W = e1 + e2
    X.A = e2 + e3
    X.wg = e2 + e3
    X.dv = e2 + e3  
    X.sy = none
    X.rem = none
    X.G = e1 + e2 + e3 + e4
    X.L = none
	X.naL = e1 + e4
	X.sloc = sq[e1+e4] + sq[e2+e3]
	X.sthd = sq[e1+e2] + sq[e3+e4]
    X.swg = sq[e1+e2] + sq[e3+e4]
    X.sdv = sq[e1 + e2 + e3 + e4]
	X.acq = e3
	X.rel = e2
	X.F = none
    X.sb = (e1 -> e2) + (e3 -> e4) 
    X.cd = (e3 -> e4)
	X.ad = none -> none
	X.dd = none -> none
    X.co = none -> none
    X.rf = (e2 -> e3)
  }

}

pred atom_hint [X:M1/Exec_OpenCL] {

  some disj e1, e2 : E {
    X.ev = e1 + e2
    X.R = e1
    X.W = e1 + e2
    X.A = e1 + e2
    X.wg = e1 + e2
	X.dv = e2
    X.sy = none
    X.rem = e2
	X.naL = none
	X.sloc = sq[e1+e2]
	X.sthd = sq[e1] + sq[e2]
    X.swg = sq[e1] + sq[e2]
    X.sdv = sq[e1 + e2]
	//X.acq = e2
	//X.rel = e2
	X.F = none
    X.sb = none->none 
    X.cd = none->none
	X.ad = none->none
	X.dd = none->none
    X.co = (e2->e1)
    X.rf = none->none
  }

}

pred p1 [X : M1/Exec_OpenCL, map: E -> M2/Inst] {
 /* aiming for mp */
  no X.(R&W)
  no X.rem
  gp[X,map]
}

pred p2 [X : M1/Exec_OpenCL, map: E -> M2/Inst] {
  /* aiming for atomicity violation */
  gp[X,map]
}

run p1 for /* aiming for mp */
// Benjamin, plingeling, with SW hint: 1 min 14 seconds.
// Benjamin, plingeling, without hint: 73 min
// Babillion, plingeling, with SW hint: 1 min 44 seconds.
// Babillion, plingeling, with HW&SW hint: 16 seconds.
exactly 1 M1/Exec, 
4 E,
3 GState,
7 LState,
5 MemEntry,
2 Val,
2 Loc,
10 Action
expect 1 // 1min (benjamin, glucose)


run p2 for /* aiming for atomicity violation */
// Babillion, plingeling, without hint: 2 min 7 seconds.
exactly 1 M1/Exec,
2 E,
4 GState,
6 LState,
6 MemEntry,
3 Val,
1 Loc,
9 Action
expect 1 // 1min (benjamin, glucose)
