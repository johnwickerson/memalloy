open ../archs/exec_OCaml[SE] as SW
open ../archs/exec_ppc[HE] as HW

/*
An OCaml-to-Power mapping.
*/

module ocaml_ppc_v1[SE,HE]

pred apply_map[X:SW/Exec_OCaml, X':HW/Exec_PPC, map:SE->HE] {

  X.EV = SE
  X'.EV = HE
    
  // two SW events cannot be compiled to a single HW event
  map in X.EV lone -> X'.EV
   
  // HW reads/writes cannot be invented by the compiler
  all e : X'.(R+W) | one e.~map

  // SW reads/writes cannot be discarded by the compiler
  all e : X.(R+W) | some e.map
  
  // a non-atomic read compiles to a single read followed by a ctrl dep
  all e : X.((R - W) - A) | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in X'.cd   
  }
      
  // an atomic read compiles to a sync, read, sync.
  all e : X.((R - W) & A) | let e1 = e.map {
    one e1 
    e1 in X'.R
    (X'.sb) :> e1 in sync[none,X']
    e1 <: (X'.sb) in sync[none,X']
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
    (X'.sb) :> e1 in sync[none,X']
    e1 <: (X'.sb) in sync[none,X']
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
  X.sloc = map . (X'.sloc) . ~map
    
  // ctrl dependencies are preserved (but more may be introduced)
  X.cd in map . (X'.cd) . ~map

  // the mapping preserves threads
  X.sthd = map . (X'.sthd) . ~map
  
}
