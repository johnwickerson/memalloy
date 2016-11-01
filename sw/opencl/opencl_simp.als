/* Manually generated from opencl.cat */

module opencl_simp[E]
open opencl_base[E]

pred Ssimp[x : Exec_OpenCL] {
  let scb = x.co + fr[x] + (hb[x]) |
  let FscbF = (rc[Fsb[x]]) . scb . (rc[sbF[x]]) |
  let scp = FscbF & (x.sc -> x.sc) - iden |
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
