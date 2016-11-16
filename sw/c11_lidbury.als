/* Based on the Lidbury-Donaldson POPL'17 axiomatic model */

module c11_partial[E]
open c11_base[E]

fun scp[x : Exec_C] : E->E {
  let S1 = hb[x] |
  let S2 = (rc[Fsb[x]]) . (x.co) . (rc[sbF[x]]) |
  let S3 = ~(x.rf) . (stor[x.sc]) . (x.co) |
  let S4 = ~(x.rf) . (hbl[x]) . (stor[x.W]) |
  let S44 = ((stor[x.R]) - (~(x.rf) . (x.rf))) . (fr[x]) |
  let S5 = (Fsb[x]) . (fr[x]) |
  let S6 = (fr[x]) . (sbF[x]) |
  let S7 = (Fsb[x]) . (fr[x]). (sbF[x]) |
  let Sall = S1+S2+S3+S4+S44+S5+S6+S7 |
  Sall & (x.sc -> x.sc) - iden
}

pred consistent[x : Exec_C] {
  HbCom[x]
  NaRf[x]
  is_acyclic[scp[x]]
  is_acyclic[scp[x] + x.sb + x.rf + x.co]
}
