/*
A C11-to-x86 mapping that implements SC atomics using 
atomic hardware events. 
*/

open ../sw/exec_C[SE] as SW
open ../hw/cpu/exec_x86[HE] as HW

module c11_x86a[SE,HE]

pred apply_map[X : SW/Exec_C, X' : HW/Exec_X86, map : SE -> HE] {

  X.ev = SE
  X'.ev = HE

  // every software event is mapped to exactly one hardware event
  map in X.ev one -> one X'.ev

  // there are no no-ops on the software side
  X.ev in X.(R + W + F)

  // RMWs are not considered
  no (X.(R&W))
  no (X'.(R&W))
  no X'.atom

  // Reads compile to reads
  (X.R).map = X'.R

  // Writes compile to writes
  (X.W).map = X'.W

  // SC events compile to atomic events
  (X.sc).map = X'.locked

  // SC fences compile to fences
  (X.(F & sc)).map in X'.F

  // Non-SC fences compile to no-ops
  (X.(F - sc)).map in X'.(ev - (R + W + F))
 
  // the mapping preserves sb
  (X.sb).map = map.(X'.sb)

  // the mapping preserves dependencies
  (X.cd).map = map.(X'.cd)
  (X.ad).map = map.(X'.ad)
  (X.dd).map = map.(X'.dd)
 
  // the mapping preserves rf
  (X.rf).map = map.(X'.rf)

  // the mapping preserves co
  (X.co).map = map.(X'.co)

  // the mapping preserves threads
  (X.sthd).map = map.(X'.sthd)

  // the mapping preserves loc
  (X.sloc).map = map.(X'.sloc)

}
