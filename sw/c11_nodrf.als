/* A weaker version of c11_orig that can be shown to violate the
   "DRF-for-SC" guarantee.
*/

module c11_orig_nodrf[E]
open c11_base[E]

pred consistentS[x : Exec_C, s : E -> E] {
  irreflexive[s . (hb[x])]                                          // S1
  irreflexive[s . (rc[Fsb[x]]) . (x.co) . (rc[sbF[x]])]             // S2
  irreflexive[s . ~(x.rf) . (stor[x.sc]) . (x.co)]                  // S3
//irreflexive[s_imm . ~(x.rf) . (hbl[x]) . (stor[x.W])]             // S4
//irreflexive[s_imm . ((stor[x.R]) - (~(x.rf) . (x.rf))) . (fr[x])] // S44
  irreflexive[s . (Fsb[x]) . (fr[x])]                               // S5
  irreflexive[s . (fr[x]) . (sbF[x])]                               // S6
  irreflexive[s . (Fsb[x]) . (fr[x]). (sbF[x])]                     // S7  
}

pred consistent[x : Exec_C] {
  some s : E -> E {
    wf_s[x,s]
    consistentS[x,s]
  }
  HbCom[x]
  NaRf[x]
}
