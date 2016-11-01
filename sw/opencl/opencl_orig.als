/* Manually generated from opencl.cat */

module opencl_orig[E]
open opencl_base[E]

pred S_orig[x : Exec_OpenCL] {
  let S1 = hb[x] |
  let S2 = (rc[Fsb[x]]) . (x.co) . (rc[sbF[x]]) |
  let S3 = ~(x.rf) . (stor[x.sc]) . (x.co) |
  let S4 = ~(x.rf) . (hb[x] & x.sloc) . (stor[x.W]) |
  let S44 = ((stor[x.R]) - (~(x.rf) . (x.rf))) . (fr[x]) |
  let S5 = (Fsb[x]) . (fr[x]) |
  let S6 = (fr[x]) . (sbF[x]) |
  let S7 = (Fsb[x]) . (fr[x]). (sbF[x]) |
  let Sall = S1 + S2 + S3 + S4 + S44 + S5 + S6 + S7 |
  let scp = Sall & (x.sc -> x.sc) - iden |
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
  S_orig[x]
}
