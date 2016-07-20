open ../../mappings/c11_strengthening1[E]
open ../../sw/c11_simp[E] as M1

sig E {}

pred gp [
  X, X' : Exec_C, map : E->E
] {
        
  // we have a valid application of the mapping
  apply_map_c11[X, X']

  map = (iden :> X.ev)

  // The "weak" execution is inconsistent ...
  not(M1/consistent[X])
  M1/dead[X]

  // The "strong" execution is consistent...
  M1/consistent[X']
  
  // optional extra
  M1/racefree[X']

  // Prefer solutions without RMWs
  no_RMWs[X]

}


run gp for 2 Exec, 5 E expect 0 // 3min (glucose, benjamin)

run gp for 2 Exec, 6 E expect 1  // 11s (plingeling, benjamin)
// same as in Vafeiadis et al POPL 2015.
