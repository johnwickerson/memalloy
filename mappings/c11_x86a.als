open ../archs/exec_C[SE] as SW
open ../archs/exec_x86[HE] as HW

/*
A C11-to-x86 mapping that implements SC atomics using 
atomic hardware events. Currently broken.
*/

module c11_x86a[SE,HE]

fun myfr_init [X:HW/Exec_X86] : HE->HE {
  ((stor[X.R]) - ((~(X.rf)) . (X.rf))) . (X.sloc) . (stor[X.W])
}

fun myfr [X:HW/Exec_X86] : HE->HE {
  (((myfr_init[X]) + ((~(X.rf)) . (X.co))) - iden)
}

pred apply_map[X:SW/Exec_C, X':HW/Exec_X86, map:SE->HE] {

  // two SW events cannot be compiled to a single HW event
  map in X.EV lone -> X'.EV
    
  // HW reads/writes cannot be invented by the compiler
  all e : X'.(R+W) | one e.~map

  // SW reads/writes cannot be discarded by the compiler
  all e : X.(R+W) | some e.map

  // a read compiles to a single non-locked read
  all e : X.(R - W) {
    one e.map
    e.map in X'.R - (X'.atom).univ
  }
    
  // a non-SC write compiles to a single non-locked write
  all e : X.((W - R) - SC) {
    one e.map
    e.map in X'.W - univ.(X'.atom)
  }

  // an SC write compiles to an RMW
  all e : X.((W - R) & SC) | some disj e1,e2 : X'.EV {
    e.map = e1 + e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]

    // read does not observe a too-late value
    (e2 -> e1) not in ((X'.co) . (X'.rf))

    // read does not observe a too-early value
    (e1 -> e2) not in ((myfr[X']) . (X'.co))
  }

  // RMWs compile to locked RMWs
  all e : X.(R & W) | some disj e1, e2 : X'.EV {
    e.map = e1 + e2
    e1 in X'.R
    e2 in X'.W
    (e1 -> e2) in X'.atom & imm[X'.sb]
  }

  // SC fences compile to full fences
  all e : X.(F & SC) {
    (X.sb) . (stor[e]) . (X.sb) = map . (mfence[none->none,X']) . ~map
  }
 
  // sb edges are preserved (but more may be introduced)
  X.sb in map . (X'.sb) . ~map
  
  // rf edges (except those entering the RMWs induced
  // by SC writes) are preserved
  let newR = X'.R & map[X.((W - R) & SC)] |
  X'.rf - (X'.EV -> newR) = ~map . (X.rf) . map

  // the mapping preserves co
  X.co = map . (X'.co) . ~map

  // the mapping preserves address dependencies
  X.ad = map . (X'.ad) . ~map

  // the mapping preserves data dependencies
  X.dd = map . (X'.dd) . ~map

  // the mapping preserves locations
  X.sloc = map . (X'.sloc) . ~map
    
  // the mapping preserves control dependencies
  X.cd in map . (X'.cd) . ~map

  // the mapping preserves threads
  X.sthd = map . (X'.sthd) . ~map

}
