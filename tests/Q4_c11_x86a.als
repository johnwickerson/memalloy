open ../mappings/c11_x86a[SE,HE]
open ../models/c11_simp[SE] as M1
open ../models/x86tso[HE] as M2

sig SE, HE {}

pred gp [X : Exec_C, X' : Exec_X86, map: SE -> HE] {

  withoutinit[X]
  withoutinit[X']
    
  // we have a valid application of the mapping
  apply_map[X, X', map]

  // The execution is forbidden in software ...
  not(M1/consistent[none,X])
  M1/dead[none,X]

  // ... but can nonetheless be observed on the hardware.
  M2/consistent[none,X']
  
}

run gp for
exactly 1 c11_x86a/SW/exec/Exec,
exactly 1 c11_x86a/HW/exec_H/exec/Exec,
4 HE, 
4 SE
expect 0
