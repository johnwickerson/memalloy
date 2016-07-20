/*
A x86-to-x86 mapping that allows:
- sb edges to be introduced,
- threads to be interleaved,
- dependencies to be introduced,
- non-locked operations to become locked
*/

open ../hw/cpu/exec_x86[E]
open strengthening[E]
module x86_strengthening[E]

pred apply_map_x86[X, X' : Exec_X86] { 

  apply_map[X,X']

  // RMWs are not considered
  no (X.(R&W))
  no (X'.(R&W))
  no X.atom
  no X'.atom

  // the strengthened execution may contain *more* locked events
  X.locked in X'.locked
}
