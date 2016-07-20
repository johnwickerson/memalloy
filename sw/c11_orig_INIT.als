/* Manually generated from c11_orig.cat. Adapted from
   POPL'16 version so as *not* to use init writes.
*/

module c11_orig_INIT[E]
open c11_base_INIT[E]

pred consistentS[x : Exec_C, s : E -> E] {
  let s_imm = s - ((x.co) . s) |
  let S1 = irreflexive[s . (hb[x])] |
  let S2 = irreflexive[s . (rc[Fsb[x]]) . (x.co) . (rc[sbF[x]])] |
  let S3 = irreflexive[s . ~(x.rf) . (stor[x.sc]) . (x.co)] |
  let S4 = irreflexive[s_imm . ~(x.rf) . (hbl[x]) . (stor[x.W])] |
  let S5 = irreflexive[s . (Fsb[x]) . (fr[x])] |
  let S6 = irreflexive[s . (fr[x]) . (sbF[x])] |
  let S7 = irreflexive[s . (Fsb[x]) . (fr[x]). (sbF[x])] {
    S1
    S2
    S3
    S4
    S5
    S6
    S7   
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
    
pred racefree[x : Exec_C] {
  Dr[x]
  Ur[x]
}
