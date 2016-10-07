open ../../mappings/opencl_ptx2[SE,HE]
open ../../sw/opencl/opencl_orig[SE] as M1
open ../../hw/gpu/ptx_cummulative[HE] as M2

sig SE, HE {}

pred gp [X : Exec_OpenCL, X' : Exec_PTX, map: SE -> HE] {
        
  // we have a valid application of the mapping
  apply_map[X, X', map]

  // The execution is forbidden in software ...
  not(M1/consistent[X])
  M1/dead[X]

  // ... but can nonetheless be observed on the hardware.
  M2/consistent[X']

  // atom relation not worked out yet
  no X'.atom

}

run gp for
exactly 1 M2/exec_ptx/exec_H/exec/Exec,
exactly 1 opencl_ptx2/opencl_ptx_base/SW/exec_C/exec/Exec,
12 HE, 5 SE, 4 Int