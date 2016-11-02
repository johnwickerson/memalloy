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
  not(M1/consistent[X])
  M1/dead[X]

  // But the "strong" execution is consistent (and not faulty)...
  M1/consistent[X']
}

run gp for exactly 2 Exec, 5 E, 3 Int expect 1
/* This finds an example of non-monotonicity in x86tso.cat 
   (1 second, plingeling, Babillion).
   Upgrading a read from being unlocked (i.e. an ordinary read)
   to being locked (i.e. the result of a failed atomic instruction)
   results in an execution becoming allowed.
   This may indicate a bug in the model.
*/

