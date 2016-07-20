open ../../hw/gpu/ptx[E] as M2
open ../../hw/gpu/ptx_cummulative[E] as M1

sig E {}

pred gp [X : Exec_PTX] {

  no X.(R&W)

  no ((X.ev -> X.ev) - X.sgl)

  no X.atom

  // The execution is forbidden in M1
  not(M1/consistent[X])
  M1/dead[X]

  // The execution is allowed (and not faulty) in M2
  M2/consistent[X]

}

run gp for exactly 1 Exec, 7 E 
