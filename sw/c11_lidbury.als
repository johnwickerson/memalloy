/* Based on the Lidbury-Donaldson POPL'17 axiomatic model */

module c11_partial[E]
open c11_base[E]

fun scp[e:E, x : Exec_C] : E->E {
  let S1 = hb[e,x] |
  let S2 = (rc[Fsb[e,x]]) . (co[e,x]) . (rc[sbF[e,x]]) |
  let S3 = ~(rf[e,x]) . (stor[sc[e,x]]) . (co[e,x]) |
  let S4 = ~(rf[e,x]) . (hbl[e,x]) . (stor[W[e,x]]) |
  let S44 = ((stor[R[e,x]]) - (~(rf[e,x]) . (rf[e,x]))) . (fr[e,x]) |
  let S5 = (Fsb[e,x]) . (fr[e,x]) |
  let S6 = (fr[e,x]) . (sbF[e,x]) |
  let S7 = (Fsb[e,x]) . (fr[e,x]). (sbF[e,x]) |
  let Sall = S1+S2+S3+S4+S44+S5+S6+S7 |
  Sall & (sc[e,x] -> sc[e,x]) - iden
}

pred consistent[e:E, x : Exec_C] {
  HbCom[e,x]
  NaRf[e,x]
  is_acyclic[scp[e,x]]
  is_acyclic[scp[e,x] + sb[e,x] + rf[e,x] + co[e,x]]
}
