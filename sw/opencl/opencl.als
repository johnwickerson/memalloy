/* Manually generated from opencl.cat */

module opencl[E]
open opencl_base[E]

pred Ssimp[x : Exec_OpenCL] {
  let scp = ((rc[Fsb[x]]) . (x.co + fr[x] + (hb[x])) .
	     (rc[sbF[x]])) & (x.sc -> x.sc) - iden |
  let unv = x.ev -> x.ev |
  let s_cond1 = unv - (unv . (stor[x.(sc - (sy & fga))]) . unv) |
  let s_cond2 = unv - (unv . (stor[x.(sc - (dv - fga))]) . unv) |
  is_acyclic[scp & (s_cond1 + s_cond2)]
}

pred consistent[x : Exec_OpenCL] {
  Ghb[x]
  Lhb[x]
  Gcoh[x]
  Lcoh[x]
  Rf[x]
  GnaRf[x]
  LnaRf[x]
  Rmw[x]
  Ssimp[x]
}		      		      

pred racefree[x : Exec_OpenCL] {
  Dr[x]
  Bd[x]
}


