/*
A C11-to-Arm7 mapping. Currently this is incorrect -- it's actually
using the Power mapping, but Arm7 fences!
*/

open ../archs/exec_C[SE] as SW
open ../archs/exec_arm7[HE] as HW

module c11_arm7_trimmed[SE,HE]

pred apply_map[
  X : SW/Exec_C,ad,cd,dd:SE->SE,
  X' : HW/Exec_Arm7,ad',cd',dd',dmbst',dmbld',dmb',isb':HE->HE, 
  map : SE -> HE
] {

  // every software event is mapped to at least one hardware event
  map in X.ev one -> some X'.ev

  // there are no no-ops on the software side
  X.ev in X.(R + W + F)
  
  // a non-atomic or relaxed read compiles to a single read
  all e : X.((R - W) - acq) {
    one e.map
    e.map in X'.R
  }
      
  // an acquire read compiles to a read followed by an isb, with
  // control dependencies inserted between the read and every
  // event that is sequenced after it
  all e : X.((R - W) & acq - sc) | let e1 = e.map {
    one e1 
    e1 in X'.R
    e1 <: (X'.sb) in cd' & isb'
  }
   
  // an SC read compiles to a read followed by a dmb
  all e : X.((R - W) & sc) | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in dmb'
  }
  
  // a non-atomic or relaxed write compiles to a single write
  all e : X.((W - R) - rel) {
    one e.map
    e.map in X'.W
  }

  // a release write compiles to a dmb followed by a write
  all e : X.((W - R) & rel - sc) | let e1 = e.map {
    one e1
    e1 in X'.W
    (X'.sb) :> e1 in dmb'
  }
  
  // an SC write compiles to a dmb followed by a write followed by a dmb
  all e : X.((W - R) & sc) | let e1 = e.map {
    one e1
    e1 in X'.W
    e1 <: (X'.sb) in dmb'
    (X'.sb) :> e1 in dmb'
  }
  
  // a relaxed RMW compiles to a read followed by a write, with 
  // control dependencies inserted between the read and every 
  // event that is sequenced after it
  all e : X.((R & W) - (acq + rel)) | some disj e1,e2 : X'.ev {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    e1 <: (X'.sb) in cd'
  }

  // an acquire RMW compiles to a read followed by a write, 
  // followed by an isb, with control dependencies inserted 
  // between the read and every event that is sequenced after it
  all e : X.((R & W & acq) - (rel + sc)) | some disj e1,e2 : X'.ev {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    e1 <: (X'.sb) in cd'
    e2 <: (X'.sb) in isb'
  }

  // a release RMW compiles to a dmb, followed by a read, followed by
  // a write, with control dependencies inserted between the read and
  // every event that is sequenced after it
  all e : X.((R & W & rel) - (acq + sc)) | some disj e1,e2 : X'.ev {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    (X'.sb) :> e1 in dmb'
    e1 <: (X'.sb) in cd'
  }

  // an acquire/release RMW compiles to a dmb, followed by a read,
  // followed by a write, followed by an isb, with control dependencies
  // inserted between the read and every event that is sequenced after
  // it
  all e : X.((R & W & rel & acq) - sc) | some disj e1,e2 : X'.ev {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    (X'.sb) :> e1 in dmb'
    e1 <: (X'.sb) in cd'
    e2 <: (X'.sb) in isb'
  }

  // an SC RMW compiles to a dmb, followed by a read, followed by
  // a write, followed by a dmb, with control dependencies inserted
  // between the read and every event that is sequenced after it
  all e : X.(R & W & sc) | some disj e1,e2 : X'.ev {
    e.map = e1+e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
    (X'.sb) :> e1 in dmb'
    e1 <: (X'.sb) in cd'
    e2 <: (X'.sb) in dmb'
  }

  // release/acquire fences compile to dmbs
  all e : X.(F & (acq + rel) - sc) {
    (X.sb) . (stor[e]) . (X.sb) = map . dmb' . ~map
  }
     
  // SC fences compile to dmbs
  all e : X.(F & sc) {
    (X.sb) . (stor[e]) . (X.sb) = map . dmb' . ~map
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
  (X.sloc) = map . (X'.sloc) . ~map
    
  // ctrl dependencies are preserved (but more may be introduced)
  cd in map . cd' . ~map

  // the mapping preserves threads
  (X'.sthd) = ~map . (X.sthd) . map
  
}
