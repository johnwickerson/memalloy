/*
An OCaml-to-Power mapping.
*/

open ../archs/exec_OCaml[SE] as SW
open ../archs/exec_ppc[HE] as HW

module ocaml_ppc_v1[SE,HE]

pred apply_map[
  X : SW/Exec_OCaml, ad,cd,dd:SE->SE,
  X' : HW/Exec_PPC, ad',cd',dd',sync',lwsync',eieio',isync':HE->HE, 
  map : SE -> HE
] {

  // every read/write event in SW is mapped to at least one HW event
  map in X.(R+W) one -> some X'.ev

  // there are no no-ops
  X.ev in X.(R + W + F)
  X'.ev in X'.(R + W)
  
  // a non-atomic read compiles to a single read
  all e : X.((R - W) - A) {
    one e.map
    e.map in X'.R
  }
      
  // an atomic read compiles to a sync, read, sync.
  all e : X.((R - W) & A) | let e1 = e.map {
    one e1 
    e1 in X'.R
    (X'.sb) :> e1 in sync'
    e1 <: (X'.sb) in sync'
  }
  
  // a non-atomic write compiles to a single write
  all e : X.((W - R) - A) {
    one e.map
    e.map in X'.W
  }

  // an atomic write compiles to a sync, write, sync
  all e : X.((W - R) & A) | let e1 = e.map {
    one e1
    e1 in X'.W
    (X'.sb) :> e1 in sync'
    e1 <: (X'.sb) in sync'
  }
 
  // sb edges are preserved (but more may be introduced)
  X.sb in map . (X'.sb) . ~map
  
  // the mapping preserves rf
  X.rf = map . (X'.rf) . ~map

  // the mapping preserves co
  X.co = map . (X'.co) . ~map

  // the mapping preserves address dependencies
  ad = map . ad' . ~map

  // the mapping preserves data dependencies
  dd = map . dd' . ~map

  // the mapping preserves locations
  X.sloc = map . (X'.sloc) . ~map
    
  // ctrl dependencies are preserved (but more may be introduced)
  cd in map . cd' . ~map

  // the mapping preserves threads
  X.sthd = map . (X'.sthd) . ~map
  
}
