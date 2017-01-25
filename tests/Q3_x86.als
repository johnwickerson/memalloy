open ../mappings/x86_strengthening[E]
open ../hw/x86tso[E] as M1

sig E {}

pred gp [X, X' : Exec_X86, map : E->E] {

  map = (iden :> X.ev)

  // there are no spare events
  E in X.ev + X'.ev

  // prefer symmetric solution
  //#(X.R) = #(X.W)
        
  // we have a valid application of the mapping
  apply_map_x86[X, X']

  // The "weak" execution is inconsistent ...
  not(M1/consistent[none,X])
  M1/dead[none,X]

  // But the "strong" execution is consistent (and not faulty)...
  M1/consistent[none,X']
}

run gp for exactly 2 Exec, 6 E, 3 Int expect 0 // 3 mins with Glucose


