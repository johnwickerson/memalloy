open ../archs/exec_C[E]

open strengthening[E]

/*
A C11-to-C11 mapping that (only) allows memory orders to be strengthened.
*/

module c11_strengthening_mo[E]

pred apply_map[X,X':Exec_C] { 

  strengthening[X,X']

  // memory orders are kept the same or strengthened
  X.ACQ in X'.ACQ
  X.REL in X'.REL
  X.SC in X'.SC

  // the mapping preserves naL in both directions
  X.NAL = X'.NAL

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
