open ../mappings/opencl_ptx[SE,HE]
open ../sw/opencl/opencl_simp[SE] as M1
open ../hw/ptx_orig[HE] as M2

sig SE, HE {}

pred gp [X : Exec_OpenCL, X' : Exec_PTX, map: SE -> HE] {
        
  // we have a valid application of the mapping
  apply_map[X, X', map]

  // The execution is forbidden in software ...
  not(M1/consistent[none,X])
  M1/dead[none,X]

  // ... but can nonetheless be observed on the hardware.
  M2/consistent[none,X']

  // atom relation not worked out yet
  no X'.atom

}

run gp for
exactly 1 M2/exec_ptx/exec_H/exec/Exec,
exactly 1 M1/opencl_base/exec_OpenCL/exec_C/exec/Exec,
7 HE, 5 SE, 4 Int expect 0 // 3 mins (Glucose)

run gp for
exactly 1 M2/exec_ptx/exec_H/exec/Exec,
exactly 1 M1/opencl_base/exec_OpenCL/exec_C/exec/Exec,
8 HE, 5 SE, 4 Int expect 1 // 12 seconds (Glucose)
