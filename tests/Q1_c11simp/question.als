open ../../sw/c11_simp[E] as M1

sig E {}

pred gp [X : Exec_C] {

  // The execution is forbidden
  not(M1/consistent[X])
  M1/dead[X]

  // Prefer solutions without RMWs
  no_RMWs[X]    
	 
  // Prefer solutions with total sb per thread
  total_sb[X]

}

run gp for 1 Exec, 2 E 
// Soln in 63ms + 110ms (plingeling on babillion)
