open ../../archs/fences_as_relations/exec_C[SE] as SW
open ../../archs/fences_as_relations/exec_arm8[HE] as HW

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
  all e : X.R {
    one e.map
    e.map in (e in X.ACQ => X'.(R & SCACQ) else X'.(R - SCACQ))
  }

  // a relaxed read induces a ctrl dependency (needed for Lahav et al.'s C++ mm)
  all e : X.R & X.A - X.ACQ {
    e.map <: (X'.sb) in (X'.cd)
  }

  // a write compiles to a write (matching REL/SCREL)
  all e : X.W {
    one e.map
    e.map in (e in X.REL => X'.(W & SCREL) else X'.(W - SCREL))
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

  // the mapping preserves rmw-edges (and inserts ctrl edges)
  X.atom = map . (X'.atom & X'.cd) . ~map

}
