open ../sw/c11_simp[E] as M1
open ../sw/c11_orig[E] as M2

sig E {}

pred gp [X : Exec_C] {     

  // The execution is forbidden in M1
  not(M1/consistent[X])
  M1/dead[X]

  // The execution is allowed (and not faulty) in M2
  M2/consistent[X]

  // Prefer fewer RMWs
  //no_RMWs[X]
  lone (X.(R&W)) 

  // Prefer solutions with total sb per thread
  //total_sb[X]  
    
}

run gp for exactly 1 Exec, 4 E expect 0
// <1s

run gp for exactly 1 Exec, 5 E expect 1
// Soln is similar to Batty et al. (POPL'16) but quite a bit simpler.
// <1s


