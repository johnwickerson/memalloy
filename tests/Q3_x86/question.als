open ../../mappings/x86_strengthening[E]
open ../../hw/cpu/x86tso[E] as M1

sig E {}

pred gp [X, X' : Exec_X86, map : E->E] {

  map = (iden :> X.ev)

  // there are no spare events
  E in X.ev + X'.ev

  // prefer symmetric solution
  #(X.R) = #(X.W)
        
  // we have a valid application of the mapping
  apply_map_x86[X, X']

  // The "weak" execution is inconsistent ...
  not(M1/consistent[X])
  M1/dead[X]

  // But the "strong" execution is consistent (and not faulty)...
  M1/consistent[X']

  // atom relation not working yet
  no X.atom
  no X'.atom
}

run gp for exactly 2 Exec, 4 E, 3 Int expect 1
/* This finds a bug in x86tso.cat (1 second, plingeling, Babillion).
   All-atomic store buffering is allowed, but partially-atomic 
   store buffering is *not* allowed.
*/

