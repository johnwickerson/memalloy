/*
An OpenCL-to-PTX mapping
*/

open ../archs/exec_OpenCL[SE] as SW
open ../archs/exec_ptx[HE] as HW
module opencl_ptx[SE,HE]

fun mk_fence[e: SE,
    X : SW/Exec_OpenCL,
    membar_cta',membar_gl',membar_sys' : HE->HE] : HE -> HE
{
  (e in X.sy) => membar_sys' else
  (e in X.dv) => membar_gl' else
                 membar_cta'
}

pred apply_map[
  X : SW/Exec_OpenCL,
  ad,cd,dd : SE->SE,
  X' : HW/Exec_PTX,
  ad',cd',dd',membar_cta',membar_gl',membar_sys' : HE->HE,
  map : SE -> HE]
{

  // We don't consider RMWs in this mapping (because the test data
  // does not contain RMWs)
  no X.(R&W)
    
  // every read/write event in SW is mapped to exactly one HW event
  map in X.(R+W) one -> one X'.ev

  // there are no no-ops on the software side
  X.ev in X.(R + W + F)
  X'.ev in X'.(R + W)

  // no remote scope promotion here
  no X.rem

  // We don't consider local memory in this mapping because ASPLOS
  // model didn't discuss local memory (we could probably figure
  // this out though)
  no X.L

  // No inter-device stuff
  no X.sy
  no (X.ev->X.ev) - X.sdv

  // map fences
  all e : X.F {
    (X.sb) . (stor[e]) . (X.sb) = map . (mk_fence[e,X,membar_cta',membar_gl',membar_sys']) . ~map
  }

  // a non-atomic or relaxed write compiles to a normal write
  all e : X.(W - rel) {
    one e.map
    e.map in X'.W
  }

  // a release write compiles to a fence followed by a write
  all e : X.(W & (rel - sc)) | let e1 = e.map {
    one e1
    e1 in X'.W
    (X'.sb) :> e1 in mk_fence[e,X,membar_cta',membar_gl',membar_sys']
  }

  // a non-atomic read compiles to a regular read
  all e : X.(R - A) {
    one e.map
    e.map in X'.R
  }

  // a relaxed/acquire read compiles to a read followed by a fence
  all e : X.(R & (A - sc)) | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in mk_fence[e,X,membar_cta',membar_gl',membar_sys']
  }

  // an SC read compiles to a fence followed by a
  // read followed by a fence
  all e : X.(R & sc) | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in mk_fence[e,X,membar_cta',membar_gl',membar_sys']
    (X'.sb) :> e1 in mk_fence[e,X,membar_cta',membar_gl',membar_sys'] //comment out for buggy mapping
  }

  // an SC write compiles to a fence followed by a
  // write followed by a fence
  all e : X.(W & sc) | let e1 = e.map {
    one e1
    e1 in X'.W
    e1 <: (X'.sb) in mk_fence[e,X,membar_cta',membar_gl',membar_sys']
    (X'.sb) :> e1 in mk_fence[e,X,membar_cta',membar_gl',membar_sys']
  }

  // map workgroup to cta (semantic) scope
  X.swg = map . (X'.scta) . ~map

  // map device to global (semantic) scope
  X.sdv = map . (X'.sgl) . ~map

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
    
  // ctrl dependencies are preserved
  cd = map . cd' . ~map

  // the mapping preserves threads
  X.sthd = map . (X'.sthd) . ~map
  

}
