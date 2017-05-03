/*
A C11-to-C11 mapping that allows:
- sb edges to be introduced,
- threads to be interleaved,
- dependencies to be introduced,
*/

open ../archs/exec_C[E]
open strengthening[E]
module c11_strengthening_seq[E]

pred apply_map_c11[X,X':Exec_C] { 

  apply_map[X,X']

  // memory orders are kept the same
  X.ACQ = X'.ACQ
  X.REL = X'.REL
  X.SC = X'.SC

  // atomicity is preserved
  X.ATO = X'.ATO

}
