open ../archs/exec_C[SE] as SW
open ../archs/exec_arm7[HE] as HW

/*
A C11-to-Arm7 mapping.
*/

module c11_arm7[SE,HE]

pred apply_map[X:SW/Exec_C, X':HW/Exec_Arm7, map:SE->HE] {

  // two SW events cannot be compiled to a single HW event
  map in X.EV lone -> X'.EV
    
  // HW reads/writes cannot be invented by the compiler
  all e : X'.(R+W) | one e.~map

  // SW reads/writes cannot be discarded by the compiler
  all e : X.(R+W) | some e.map
  
  // a non-atomic or relaxed read compiles to a single read
  all e : X.((R - W) - ACQ) {
    one e.map
    e.map in X'.R
  }
      
  // an acquire read compiles to a read followed by an isb, with
  // control dependencies inserted between the read and every
  // event that is sequenced after it
  all e : X.((R - W) & ACQ - SC) | let e1 = e.map {
    one e1 
    e1 in X'.R
    e1 <: (X'.sb) in (X'.cd) & (isb[none,X'])
  }
   
  // an SC read compiles to a read followed by a dmb
  all e : X.((R - W) & SC) | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in (dmb[none,X'])
  }
  
  // a non-atomic or relaxed write compiles to a single write
  all e : X.((W - R) - REL) {
    one e.map
    e.map in X'.W
  }

  // a release write compiles to a dmb followed by a write
  all e : X.((W - R) & REL - SC) | let e1 = e.map {
    one e1
    e1 in X'.W
    (X'.sb) :> e1 in (dmb[none,X'])
  }
  
  // an SC write compiles to a dmb followed by a write followed by a dmb
  all e : X.((W - R) & SC) | let e1 = e.map {
    one e1
    e1 in X'.W
    e1 <: (X'.sb) in (dmb[none,X'])
    (X'.sb) :> e1 in (dmb[none,X'])
  }
  
  // a relaxed RMW compiles to a read followed by a write, with 
  // control dependencies inserted between the read and every 
  // event that is sequenced after it
  all e : X.((R & W) - (ACQ + REL)) | some disj e1,e2 : X'.EV {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    e1 <: (X'.sb) in (X'.cd)
  }

  // an acquire RMW compiles to a read followed by a write, 
  // followed by an isb, with control dependencies inserted 
  // between the read and every event that is sequenced after it
  all e : X.((R & W & ACQ) - (REL + SC)) | some disj e1,e2 : X'.EV {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    e1 <: (X'.sb) in (X'.cd)
    e2 <: (X'.sb) in (isb[none,X'])
  }

  // a release RMW compiles to a dmb, followed by a read, followed by
  // a write, with control dependencies inserted between the read and
  // every event that is sequenced after it
  all e : X.((R & W & REL) - (ACQ + SC)) | some disj e1,e2 : X'.EV {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    (X'.sb) :> e1 in (dmb[none,X'])
    e1 <: (X'.sb) in (X'.cd)
  }

  // an acquire/release RMW compiles to a dmb, followed by a read,
  // followed by a write, followed by an isb, with control dependencies
  // inserted between the read and every event that is sequenced after
  // it
  all e : X.((R & W & REL & ACQ) - SC) | some disj e1,e2 : X'.EV {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    (X'.sb) :> e1 in (dmb[none,X'])
    e1 <: (X'.sb) in (X'.cd)
    e2 <: (X'.sb) in (isb[none,X'])
  }

  // an SC RMW compiles to a dmb, followed by a read, followed by
  // a write, followed by a dmb, with control dependencies inserted
  // between the read and every event that is sequenced after it
  all e : X.(R & W & SC) | some disj e1,e2 : X'.EV {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    (X'.sb) :> e1 in (dmb[none,X'])
    e1 <: (X'.sb) in (X'.cd)
    e2 <: (X'.sb) in (dmb[none,X'])
  }

  // release or acquire fences compile to dmbs
  all e : X.(F & (ACQ + REL) - SC) {
    (X.sb) . (stor[e]) . (X.sb) = map . (dmb[none,X']) . ~map
  }
     
  // SC fences compile to dmbs
  all e : X.(F & SC) {
    (X.sb) . (stor[e]) . (X.sb) = map . (dmb[none,X']) . ~map
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
  dd = map . (X'.dd) . ~map

  // the mapping preserves locations
  (X.sloc) = map . (X'.sloc) . ~map
    
  // ctrl dependencies are preserved (but more may be introduced)
  X.cd in map . (X'.cd) . ~map

  // the mapping preserves threads
  (X'.sthd) = ~map . (X.sthd) . map
  
}
