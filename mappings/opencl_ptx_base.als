/*
An OpenCL-to-PTX mapping
*/

open ../sw/opencl/exec_OpenCL[SE] as SW
open ../hw/exec_ptx[HE] as HW
module opencl_ptx_base[SE,HE]

fun map_scope[e: SE, X : SW/Exec_OpenCL, X' : HW/Exec_PTX] : set HE {
    (e in X.wg && !(e in X.dv)) =>
    X'.membar_cta
    else {
       (e in X.dv && !(e in X.sy)) =>
       X'.membar_gl
       else {
          X'.membar_sys
       }
    }
}

pred apply_map_base[
  X : SW/Exec_OpenCL, X' : HW/Exec_PTX, 
  map : SE -> HE
] {

  X.ev = SE
  X'.ev = HE

  // every software event is mapped to at least one hardware event
  map in X.ev one -> some X'.ev

  // there are no no-ops on the software side (or fences for now)
  X.ev in X.(R + W + F)

  // We don't consider RMWs in this mapping (because the test data
  // does not contain RMWs)
  no X.(R&W)

  // no remote scope promotion here
  no X.rem

  // We don't consider local memory in this mapping because ASPLOS
  // model didn't discuss local memory (we could probably figure
  // this out though)
  no X.L

  no X.sy

  no (X.ev->X.ev) - X.sdv

  //Comment this in to try and get executions with fences.
  //some X.F
  
  //Only consider intra-device interactions 
  no ((X.ev -> X.ev) - X.sdv)

  // SC atomics are not considered (yet)
  // no (X.sc)

  // map workgroup to cta (semantic) scope
  all e1, e2 : X.ev {
      e1->e2 in X.swg => e1.map->e2.map in X'.scta
  }

  // map device to global (semantic) scope
  all e1, e2 : X.ev {
      e1->e2 in X.sdv => e1.map->e2.map in X'.sgl
  }

  //map fences
  all e : (X.F) {
    one e.map
    e.map in map_scope[e,X,X']
  }

  // a non-atomic or relaxed write compiles to a normal write
  all e : X.(W - rel) {
    one e.map
    e.map in X'.W
  }


  //This should use a scoped function
  // a release write compiles to a fence followed by a write
  all e : X.(W & (rel - sc)) | some disj e1,e2 : X'.ev {
    e.map = e1 + e2
    (e1 -> e2) in imm[X'.sb]
    e1 in map_scope[e,X,X']
    e2 in X'.(W)
  }


  // a non-atomic read compiles to a regular read
  all e : X.(R - A) {
    one e.map
    e.map in X'.R
  }

  // an relaxed/acquire read compiles to a read followed by a fence
  all e : X.(R & (A - sc)) |  some disj e1,e2 : X'.ev {
    e.map = e1 + e2
    (e1 -> e2) in imm[X'.sb]
    e1 in X'.(R)
    e2 in map_scope[e,X,X']
  }

  // an SC read compiles to a fence followed by a write followed by a fence
//  all e : X.(R & sc) | some disj e1,e2,e3 : X'.ev {
//    e.map = e1 + e2 + e3
//    (e1 -> e2) in imm[X'.sb]
//    (e2 -> e3) in imm[X'.sb]
//    e1 in map_scope[e,X,X']
//    e2 in X'.(R)
//    e3 in map_scope[e,X,X']
//  }


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
