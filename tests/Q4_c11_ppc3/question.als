open ../../mappings/c11_ppc[E, E]
open ../../sw/c11_simp[E] as M1
open ../../hw/cpu/ppc3[E] as M2

sig E {}

pred gp [
  X : Exec_C, X' : Exec_PPC, 
  map: E -> E
] {
        
  withoutinit[X]
  withoutinit[X']

  // the axiomatic model of Power does not cover RMWs
  //no X.(R&W)

  // we have a valid application of the mapping
  apply_map[X, X', map]

  // The execution is forbidden in software ...
  not(M1/consistent[X])
  M1/dead[X]
      
  // ... but can nonetheless be observed on the hardware.
  M2/consistent[X']
    
}

run gp for exactly 2 Exec, 5 E expect 0 // 43 mins
run gp for exactly 2 Exec, 6 E 
