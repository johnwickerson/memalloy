open ../archs/exec_C[SE] as SW
open ../archs/exec_arm8[HE] as HW

/*
A C11-to-ARMv8 mapping.
*/

module c11_arm8[SE,HE]

pred apply_map[
  X : SW/Exec_C, X' : HW/Exec_Arm8, map:SE->HE
] {

  // two SW events cannot be compiled to a single HW event
  map in X.EV lone -> X'.EV
    
  // HW reads/writes cannot be invented by the compiler
  all e : X'.(R+W) | one e.~map

  // SW reads/writes cannot be discarded by the compiler
  all e : X.(R+W) | some e.map


  // a read compiles to a read (matching ACQ/SCACQ)
  all e : X.(R - W) {
    one e.map
    e.map in (e in X.ACQ => X'.(R & SCACQ) else X'.(R - SCACQ))
  }

  // a write compiles to a write (matching REL/SCREL)
  all e : X.(W - R) {
    one e.map
    e.map in (e in X.REL => X'.(W & SCREL) else X'.(W - SCREL))
  }

  // an RMW compiles to a read, followed by a write, with control
  // dependencies inserted between the read and every event that is
  // sequenced after it
  all e : X.(R & W) | some disj e1,e2 : X'.EV {
    e.map = e1+e2
    e1 in (e in X.ACQ => X'.(R & SCACQ) else X'.(R - SCACQ))
    e2 in (e in X.REL => X'.(W & SCREL) else X'.(W - SCREL))
    (e1 -> e2) in X'.atom & imm[X'.sb]
    e1 <: (X'.sb) in (X'.cd)
  }

  // acquire fence compiles to dmbld
  all e : X.(F & (ACQ - REL)) {
    (X.sb) . (stor[e]) . (X.sb) = map . (dmbld[none->none,X'] - dmb[none->none,X']) . ~map
  }
     
  // release/SC fence compiles to dmb
  all e : X.(F & REL) {
    (X.sb) . (stor[e]) . (X.sb) = map . (dmb[none->none,X']) . ~map
  }
    
// sb edges are preserved (but more may be introduced)
  X.sb in map . (X'.sb) . ~map
  
  // the mapping preserves rf
  X.rf = map . (X'.rf) . ~map

  // the mapping preserves co
  X.co = map . (X'.co) . ~map

  // the mapping preserves address dependencies
  X.ad = map . (X'.ad) . ~map

  // the mapping preserves data dependencies
  X.dd = map . (X'.dd) . ~map

  // the mapping preserves locations
  (X.sloc) = map . (X'.sloc) . ~map
    
  // ctrl dependencies are preserved (but more may be introduced)
  X.cd in map . (X'.cd) . ~map

  // the mapping preserves threads
  (X'.sthd) = ~map . (X.sthd) . map

  // the mapping preserves transactions
  X.stxn = map . (X'.stxn) . ~map
  X.ftxn = map . (X'.ftxn) . ~map

}
