open ../../mappings/c11_x86a[SE,HE]
open ../../sw/c11_simp[SE] as M1
open ../../hw/cpu/x86tso_fixed[HE] as M2

sig SE, HE {}

pred gp [X : Exec_C, X' : Exec_X86, map: SE -> HE] {
        
  // we have a valid application of the mapping
  apply_map[X, X', map]

  // The execution is forbidden in software ...
  not(M1/consistent[X])
  M1/dead[X]

  // ... but can nonetheless be observed on the hardware.
  M2/consistent[X']
  
  // atom relation not worked out yet
  no X'.atom
}

run gp for
exactly 1 c11_x86a/SW/exec/Exec,
exactly 1 c11_x86a/HW/exec_H/exec/Exec,
6 HE, 
6 SE


