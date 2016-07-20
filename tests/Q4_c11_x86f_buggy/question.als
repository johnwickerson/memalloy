open ../../mappings/c11_x86f_buggy[SE,HE]
open ../../sw/c11_simp[SE] as M1
open ../../hw/cpu/x86tso[HE] as M2

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
exactly 1 c11_x86f_buggy/SW/exec/Exec,
exactly 1 c11_x86f_buggy/HW/exec_H/exec/Exec,
8 HE, 
4 SE

/* 
No solution, 2s, PLingeling, Babillion
*/
