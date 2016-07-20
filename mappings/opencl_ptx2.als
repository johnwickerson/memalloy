open opencl_ptx_base[SE,HE]
module opencl_ptx2[SE,HE]


pred apply_map[
  X : opencl_ptx_base/SW/Exec_OpenCL, 
  X' : opencl_ptx_base/HW/Exec_PTX, 
  map : SE -> HE
] {

  apply_map_base[X,X',map]     

  // an SC read compiles to a read followed by a fence
  all e : X.(R & sc) | some disj e1,e2 : X'.ev {
    e.map = e1 + e2
    (e1 -> e2) in imm[X'.sb]
    e1 in X'.R
    e2 in map_scope[e,X,X']
  }

  // an SC write compiles to a fence followed by a
  // write followed by a fence
  all e : X.(W & sc) | some disj e1,e2,e3 : X'.ev {
    e.map = e1 + e2 + e3
    (e1 -> e2) in imm[X'.sb]
    (e2 -> e3) in imm[X'.sb]
    e1 in map_scope[e,X,X']
    e2 in X'.W
    e3 in map_scope[e,X,X']
  }
    
}
