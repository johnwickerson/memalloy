/*
A C11-to-x86 mapping that implements SC atomics using 
atomic hardware events. 
*/

open ../sw/exec_C[SE] as SW
open ../hw/exec_x86[HE] as HW

module c11_x86a[SE,HE]

pred apply_map[X : SW/Exec_C, X' : HW/Exec_X86, map : SE -> HE] {

  X.ev = SE
  X'.ev = HE

  // every software event is mapped to at least one hardware event
  map in X.ev one -> some X'.ev

  // all events except RMWs and SC writes compile to single hardware events
  all e : X.(ev - (R&W) - (W&sc)) | one e.map

  // there are no no-ops on the software side
  X.ev in X.(R + W + F)

  // Reads compile to reads
  (X.(R-W)).map in X'.R

  // CAS's and SC writes compile to locked events
  (X.((R&W) + (W&sc))).map = X'.locked

  // Non-SC writes compile to non-locked writes
  (X.((W-R)-sc)).map = X'.(W-locked)

  // SC writes compile to locked RMWs
  all e : X.(W&sc) | some disj e1, e2 : X'.ev {
    e.map = e1 + e2
    e1 in X'.(R & locked)
    e2 in X'.(W & locked)
    (e1 -> e2) in imm[X'.sb] & X'.atom
    
    // read does not observe a too-late value
    (e2 -> e1) not in ((X'.co) . (X'.rf))

    // read does not observe a too-early value
    (e1 -> e2) not in ((fr[none,X']) . (X'.co))
  }

  // RMWs compile to locked RMWs
  all e : X.(R&W) | some disj e1, e2 : X'.ev {
    e.map = e1 + e2
    e1 in X'.(R & locked)
    e2 in X'.(W & locked)
    (e1 -> e2) in imm[X'.sb] & X'.atom
  }

  // SC fences compile to fences
  (X.(F & sc)).map in X'.F

  // Non-SC fences compile to no-ops
  (X.(F - sc)).map in X'.(ev - (R + W + F))
 
  // the mapping preserves sb
  (X.sb).map in map.(X'.sb)

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
