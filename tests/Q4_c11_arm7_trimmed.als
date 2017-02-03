open ../mappings/c11_arm7_trimmed[SE, HE]
open ../models_als/c11_partial[SE] as M1
open ../models_als/arm7[HE] as M2

sig SE,HE {}

pred gp [
  X : Exec_C, X' : Exec_Arm7, 
  map: SE -> HE
] {
        
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
exactly 1 c11_arm7_trimmed/SW/exec/Exec,
exactly 1 c11_arm7_trimmed/HW/exec_H/exec/Exec,
8 HE, 
6 SE
