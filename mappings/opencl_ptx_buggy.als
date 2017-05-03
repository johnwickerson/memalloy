/*
An OpenCL-to-PTX mapping
*/

open ../archs/exec_OpenCL[SE] as SW
open ../archs/exec_ptx[HE] as HW
module opencl_ptx[SE,HE]

fun mk_fence[e:SE, X:SW/Exec_OpenCL, X':HW/Exec_PTX] : HE -> HE
{
  (e in X.SY) => membar_sys[none,X'] else
  (e in X.DV) => membar_gl[none,X'] else
                 membar_cta[none,X']
}

pred apply_map[X:SW/Exec_OpenCL, X':HW/Exec_PTX, map:SE->HE] {

  // We don't consider RMWs in this mapping (because the test data
  // does not contain RMWs)
  no X.(R&W)
    
  X.EV = SE
  X'.EV = HE
    
  // two SW events cannot be compiled to a single HW event
  map in X.EV lone -> X'.EV
   
  // HW reads/writes cannot be invented by the compiler
  all e' : X'.(R+W) | one e.~map

  // SW reads/writes cannot be discarded by the compiler
  all e : X.(R+W) | some e.map

  // no remote scope promotion here
  no X.REM

  // We don't consider local memory in this mapping because ASPLOS
  // model didn't discuss local memory (we could probably figure
  // this out though)
  no X.L

  // No inter-device stuff
  no X.SY
  no (X.EV->X.EV) - X.sdv

  // a non-atomic or relaxed write compiles to a normal write
  all e : X.(W - REL) {
    one e.map
    e.map in X'.W
  }

  // a release write compiles to a fence followed by a write
  all e : X.(W & (REL - SC)) | let e1 = e.map {
    one e1
    e1 in X'.W
    (X'.sb) :> e1 in mk_fence[e,X,X']
  }

  // a non-atomic read compiles to a regular read
  all e : X.(R - ATO) {
    one e.map
    e.map in X'.R
  }

  // a relaxed/acquire read compiles to a read followed by a fence
  all e : X.(R & (ATO - SC)) | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in mk_fence[e,X,X']
  }

  // an SC read compiles to a fence followed by a
  // read followed by a fence
  all e : X.(R & SC) | let e1 = e.map {
    one e1
    e1 in X'.R
    e1 <: (X'.sb) in mk_fence[e,X,X']
    //(X'.sb) :> e1 in mk_fence[e,X,X'] //comment out for buggy mapping
  }

  // an SC write compiles to a fence followed by a
  // write followed by a fence
  all e : X.(W & SC) | let e1 = e.map {
    one e1
    e1 in X'.W
    e1 <: (X'.sb) in mk_fence[e,X,X']
    (X'.sb) :> e1 in mk_fence[e,X,X']
  }

  // map fences
  all e : X.F {
    (X.sb) . (stor[e]) . (X.sb) = map . (mk_fence[e,X,X']) . ~map
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
  X.ad = map . (X'.ad) . ~map

  // the mapping preserves data dependencies
  X.dd = map . (X'.dd) . ~map

  // the mapping preserves locations
  X.sloc = map . (X'.sloc) . ~map
    
  // ctrl dependencies are preserved
  X.cd = map . (X'.cd) . ~map

  // the mapping preserves threads
  X.sthd = map . (X'.sthd) . ~map
  

}
