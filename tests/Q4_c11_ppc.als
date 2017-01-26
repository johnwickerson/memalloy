open ../mappings/c11_ppc[SE,HE]
open ../models_als/c11_partial[SE] as M1
open ../models_als/ppc_unrolled[HE] as M2

sig SE, HE {}

pred gp [
  X : Exec_C, X' : Exec_PPC, 
  map: SE -> HE
] {

  // we have a valid application of the mapping
  apply_map[X, X', map]

  // The execution is forbidden in software ...
  not(M1/consistent[none,X])
  M1/dead[none,X]
      
  // ... but can nonetheless be observed on the hardware.
  M2/consistent[none,X']
    
}

run gp for
exactly 1 c11_ppc/SW/exec/Exec,
exactly 1 c11_ppc/HW/exec_H/exec/Exec,
13 HE, 
6 SE
