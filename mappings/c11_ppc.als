open ../archs/exec_C[SE] as SW
open ../archs/exec_ppc[HE] as HW



/*
A C11-to-Power mapping.
*/

module c11_ppc[SE,HE]

pred apply_map[X:Exec_C, X':Exec_PPC, map:SE->HE] {

  X.EV = SE
  X'.EV = HE
    
  // two SW events cannot be compiled to a single HW event
  map in X.EV lone -> X'.EV
   
  // HW reads/writes cannot be invented by the compiler
  all e : X'.(R+W) | one e.~map

  // SW reads/writes cannot be discarded by the compiler
  //all e : X.(R+W) | some e.map
  
  // a non-atomic read compiles to a single read
  all e : X.R - X.A {
    one e.map
    e.map in X'.R
  }
      
  // a relaxed read compiles to read;ctrl
  all e : X.R & X.A - X.ACQ - dom[X.atom] | let e1 = e.map {
    one e1 
    e1 in X'.R
    e1 <: (X'.sb) in (X'.cd)
  }

  // an acquire read compiles to a read followed by
  // a control fence, with control dependencies inserted between the
  // read and every event that is sequenced after it.
  all e : X.R & X.ACQ - X.SC - dom[X.atom] | let e1 = e.map {
    one e1 
    e1 in X'.R
    e1 <: (X'.sb) in (X'.cd) & (isync[none->none,X'])
  }
  
  // an SC read compiles to a full fence followed by a read 
  // followed by a control fence, with control dependencies inserted 
  // between the read and every event that is sequenced after it
  all e : X.R & X.SC - dom[X.atom] | let e1 = e.map {
    one e1
    e1 in X'.R
    (X'.sb) :> e1 in (sync[none->none,X'])
    e1 <: (X'.sb) in (X'.cd) & (isync[none->none,X']) 
  }
  
  // a non-atomic or relaxed write compiles to a single write
  all e : X.W - X.REL - ran[X.atom] {
    one e.map
    e.map in X'.W
  }

  // a release write compiles to a lightweight fence followed 
  // by a write
  all e : X.W & X.REL - X.SC - ran[X.atom] | let e1 = e.map {
    one e1
    e1 in X'.W
    (X'.sb) :> e1 in (lwsync[none->none,X'])
  }
  
  // an SC write compiles to a full fence followed by a write
  all e : X.W & X.SC - ran[X.atom] | let e1 = e.map {
    one e1
    e1 in X'.W
    (X'.sb) :> e1 in (sync[none->none,X'])
  }
  
  // the read part of a relaxed or acquire RMW compiles to read;ctrl
  all e : X.R & dom[X.atom] - X.ACQ | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in X'.cd  
  }

  // the write part of a relaxed RMW compiles to a write
  all e : X.W & ran[X.atom] - X.REL | let e1 = e.map {
    one e1
    e1 in X'.W 
  }

  // the read part of an acquire RMW compiles to read;ctrl
  all e : X.R & X.ACQ & dom[X.atom] - (X.atom).(X.REL) | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in X'.cd  
  }

  // the write part of an acquire or acquire-release or SC RMW compiles to write;isync
  all e : X.W & (X.ACQ).(X.atom) | let e1 = e.map {
    one e1
    e1 in X'.W 
    e1 <: (X'.sb) in isync[none->none,X']
  }

  // the read part of a release or acquire-release RMW compiles to lwsync;read;ctrl
  all e : X.R & (X.atom).(X.REL) - X.SC | let e1 = e.map {
    one e1
    e1 in X'.R
    (X'.sb) :> e1 in lwsync[none->none,X']
    e1 <: (X'.sb) in X'.cd  
  }

  // the write part of a release RMW compiles to a write
  all e : X.W & X.REL & ran[X.atom] - (X.ACQ).(X.atom) | let e1 = e.map {
    one e1
    e1 in X'.W 
  }

  // the read part of an SC RMW compiles to sync;read;ctrl
  all e : X.R & X.SC & dom[X.atom] | let e1 = e.map {
    one e1
    e1 in X'.R
    (X'.sb) :> e1 in sync[none->none,X']
    e1 <: (X'.sb) in X'.cd  
  }

  // release or acquire fences compile to lightweight fences
  all e : X.(F & (ACQ + REL) - SC) {
    (X.sb) . (stor[e]) . (X.sb) = map . (lwsync[none->none,X']) . ~map
  }
      
  // SC fences compile to full fences
  all e : X.(F & SC) {
    (X.sb) . (stor[e]) . (X.sb) = map . (sync[none->none,X']) . ~map
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

  // the mapping preserves rmw-edges
  X.atom = map . (X'.atom) . ~map
  
}
