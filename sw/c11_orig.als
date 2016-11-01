/* Manually generated from c11_orig.cat. Adapted from
   POPL'16 version so as *not* to use init writes.
*/

module c11_orig[E]
open c11_base[E]

pred consistentS[x : Exec_C, s : E -> E] {
  let s_imm = s - ((x.co) . s) {
    irreflexive[s . (hb[x])]                                          // S1
    irreflexive[s . (rc[Fsb[x]]) . (x.co) . (rc[sbF[x]])]             // S2
    irreflexive[s . ~(x.rf) . (stor[x.sc]) . (x.co)]                  // S3
    irreflexive[s_imm . ~(x.rf) . (hbl[x]) . (stor[x.W])]             // S4
    irreflexive[s_imm . ((stor[x.R]) - (~(x.rf) . (x.rf))) . (fr[x])] // S44
    irreflexive[s . (Fsb[x]) . (fr[x])]                               // S5
    irreflexive[s . (fr[x]) . (sbF[x])]                               // S6
    irreflexive[s . (Fsb[x]) . (fr[x]). (sbF[x])]                     // S7  
  }
}

pred consistent[x : Exec_C] {
  some s : E -> E {
    wf_s[x,s]
    consistentS[x,s]
  }
  HbCom[x]
  NaRf[x]
}
