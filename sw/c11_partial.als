/* Manually generated from c11_partialSC.cat */

module c11_partial[E]
open c11_base[E]

pred Spartial[e:E, x : Exec_C] {
  let S1 = hb[e,x] |
  let S2 = (rc[Fsb[e,x]]) . (co[e,x]) . (rc[sbF[e,x]]) |
  let S3 = ~(rf[e,x]) . (stor[sc[e,x]]) . (co[e,x]) |
  let S4 = ~(rf[e,x]) . (hbl[e,x]) . (stor[W[e,x]]) |
  let S44 = ((stor[R[e,x]]) - (~(rf[e,x]) . (rf[e,x]))) . (fr[e,x]) |
  let S5 = (Fsb[e,x]) . (fr[e,x]) |
  let S6 = (fr[e,x]) . (sbF[e,x]) |
  let S7 = (Fsb[e,x]) . (fr[e,x]). (sbF[e,x]) |
  let Sall = S1+S2+S3+S4+S44+S5+S6+S7 |
  let Sp = Sall & (sc[e,x] -> sc[e,x]) - iden |
  is_acyclic[Sp]
}

pred consistent[e:E, x : Exec_C] {
  HbCom[e,x]
  NaRf[e,x]
  Spartial[e,x]
}
