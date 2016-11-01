/*
A C11-to-x86 mapping that implements SC atomics using
fences.
*/

open ../sw/exec_C[SE] as SW
open ../hw/exec_x86[HE] as HW

module c11_x86f[SE,HE]

pred apply_map[X : SW/Exec_C, X' : HW/Exec_X86, map : SE -> HE] {

  X.ev = SE
  X'.ev = HE

  // every software event is mapped to at least one hardware event
  map in X.ev one -> some X'.ev

  // there are no no-ops on the software side
  X.ev in X.(R + W + F)

  // RMWs are not considered
  no (X.(R&W))
  no (X'.(R&W))
  no (X'.atom)

  // a read compiles to a non-locked read
  all e : X.(R - W) | one e.map && e.map in X'.(R - locked)

  // a non-SC write compiles to a non-atomic write
  all e : X.((W - R) - sc) | one e.map && e.map in X'.(W - locked)
  
  // an SC write compiles to a non-atomic write followed by a fence
  all e : X.((W - R) & sc) | some disj e1,e2 : e.map {
    e.map = e1+e2
    e1 in X'.(W - locked)
    e2 in X'.F
    (e1 -> e2) in imm[X'.sb]
  }  
  
  // non-SC fences compile to no-ops
  all e : X.(F - sc) | one e.map && e.map in X'.(ev - (R + W + F))
      
  // SC fences compile to fences
  all e : X.(F & sc) | one e.map && e.map in X'.F
       
  // the mapping preserves sb in both directions (the only allowed
  // additional sb edges are those introduced as part of the
  // compilation scheme)
  (X.sb).map = map.(X'.sb - (~map . map))

  // the mapping preserves addr/ctrl/data dependencies
  (X.cd).map = map.(X'.cd)
  (X.ad).(map :> X'.(R + W)) = map.(X'.ad)
  (X.dd).(map :> X'.W) = map.(X'.dd)

  // the mapping preserves locations
  X.sloc = map.(X'.sloc).~map
    
  // the mapping preserves rf
  X.rf = map.(X'.rf).~map

  // the mapping preserves co
  X.co = map.(X'.co).~map

  // the mapping preserves threads
  (X.sthd).map = map.(X'.sthd)
}
