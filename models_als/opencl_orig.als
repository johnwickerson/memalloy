/* Manually generated from opencl.cat */

module opencl_orig[E]
open opencl_base[E]

pred S_orig[e:E, x : Exec_OpenCL] {
  let S1 = hb[e,x] |
  let S2 = (rc[Fsb[e,x]]) . (co[e,x]) . (rc[sbF[e,x]]) |
  let S3 = ~(rf[e,x]) . (stor[sc[e,x]]) . (co[e,x]) |
  let S4 = ~(rf[e,x]) . (hb[e,x] & sloc[e,x]) . (stor[W[e,x]]) |
  let S44 = ((stor[R[e,x]]) - (~(rf[e,x]) . (rf[e,x]))) . (fr[e,x]) |
  let S5 = (Fsb[e,x]) . (fr[e,x]) |
  let S6 = (fr[e,x]) . (sbF[e,x]) |
  let S7 = (Fsb[e,x]) . (fr[e,x]). (sbF[e,x]) |
  let Sall = S1 + S2 + S3 + S4 + S44 + S5 + S6 + S7 |
  let scp = Sall & (sc[e,x] -> sc[e,x]) - iden |
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
  S_orig[e,x]
}
