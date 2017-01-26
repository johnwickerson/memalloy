/* Manually generated from opencl.cat */

module opencl_simp[E]
open opencl_base[E]

pred Ssimp[e:E, x : Exec_OpenCL] {
  let scb = co[e,x] + fr[e,x] + (hb[e,x]) |
  let FscbF = (rc[Fsb[e,x]]) . scb . (rc[sbF[e,x]]) |
  let scp = FscbF & (sc[e,x] -> sc[e,x]) - iden |
  let unv = ev[e,x] -> ev[e,x] |
  let s_cond1 = unv - (unv . (stor[sc[e,x] - (sy[e,x] & fga[e,x])]) . unv) |
  let s_cond2 = unv - (unv . (stor[sc[e,x] - (dv[e,x] - fga[e,x])]) . unv) |
  is_acyclic[scp & (s_cond1 + s_cond2)]
}

pred consistent[e:E, x : Exec_OpenCL] {
  Ghb[e,x]
  Lhb[e,x]
  Gcoh[e,x]
  Lcoh[e,x]
  Rf[e,x]
  GnaRf[e,x]
  LnaRf[e,x]
  Rmw[e,x]
  Ssimp[e,x]
}
