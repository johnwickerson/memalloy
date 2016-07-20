/*
A C11-to-C11 mapping that allows:
- sb edges to be introduced,
- threads to be interleaved,
- dependencies to be introduced,
*/

open ../sw/exec_C[E]
open strengthening[E]
module c11_strengthening1[E]

pred apply_map_c11[X, X' : Exec_C] { 

  apply_map[X,X']
  // RMWs are not considered
  //no (X.(R&W))
  //no (X'.(R&W))

  // memory orders are kept the same
  X.acq = X'.acq
  X.rel = X'.rel
  X.sc = X'.sc

  // atomicity is preserved
  X.A = X'.A

}
