/*
A C11-to-C11 mapping that (only) allows memory orders to be strengthened.
*/

open ../sw/exec_C[E]
open strengthening[E]
module c11_strengthening_mo[E]

pred apply_map_c11[X, X' : Exec_C] { 

  apply_map[X,X']
  // RMWs are not considered
  //no (X.(R&W))
  //no (X'.(R&W))

  // memory orders are kept the same or strengthened
  X.acq in X'.acq
  X.rel in X'.rel
  X.sc in X'.sc

  // atomicity is preserved in both directions
  X.A = X'.A
 
  // the mapping preserves sb in both directions
  X.sb = X'.sb

  // the mapping preserves dependencies in both directions
  X.cd = X'.cd
  X.ad = X'.ad
  X.dd = X'.dd

  // the mapping preserves sthd in both directions
  X.sthd = X'.sthd
    
}
