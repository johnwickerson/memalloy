open ../archs/exec_C[E]

open strengthening[E]

/*
A C11-to-C11 mapping that allows:
- sb edges to be introduced,
- threads to be interleaved,
- dependencies to be introduced,
*/

module c11_strengthening_seq[E]

pred apply_map[X,X':Exec_C] { 

  strengthening[X,X']

  // memory orders are kept the same
  X.ACQ = X'.ACQ
  X.REL = X'.REL
  X.SC = X'.SC

  // atomicity is preserved
  X.A = X'.A
		
  // the mapping preserves naL in both directions
  X.NAL = X'.NAL
}
