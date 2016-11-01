open opencl_ptx_base[SE,HE]
module opencl_ptx[SE,HE]


fun read_or_write[e: SE, 
  X : opencl_ptx_base/SW/Exec_OpenCL, 
  X' : opencl_ptx_base/HW/Exec_PTX] : set HE 
{
    e in X.W =>
      X'.W
      else {
        X'.R
      }
}

pred apply_map[
  X : opencl_ptx_base/SW/Exec_OpenCL, 
  X' : opencl_ptx_base/HW/Exec_PTX, 
  map : SE -> HE
] {

  apply_map_base[X,X',map]     

  // an SC read/write compiles to a fence followed by a
  // read/write followed by a fence
  all e : X.((W + R) & sc) | some disj e1,e2,e3 : X'.ev {
    e.map = e1 + e2 + e3
    (e1 -> e2) in imm[X'.sb]
    (e2 -> e3) in imm[X'.sb]
    e1 in map_scope[e,X,X']
    e2 in read_or_write[e,X,X']
    e3 in map_scope[e,X,X']
  }

}
