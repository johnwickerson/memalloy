// Exec_C

/* Manually generated from c11_orig.cat. Adapted from
   POPL'16 version so as *not* to use init writes.
*/

module c11_orig[E]
open c11_base[E]

pred consistentS[e:E, x : Exec_C, s : E -> E] {
  let s_imm = s - ((co[e,x]) . s) {
    irreflexive[s . (hb[e,x])]                                       // S1
    irreflexive[s . (rc[Fsb[e,x]]) . (co[e,x]) . (rc[sbF[e,x]])]     // S2
    irreflexive[s . ~(rf[e,x]) . (stor[sc[e,x]]) . (co[e,x])]        // S3
    irreflexive[s_imm . ~(rf[e,x]) . (hbl[e,x]) . (stor[W[e,x]])]    // S4
    irreflexive[s_imm . ((stor[R[e,x]]) - (~(rf[e,x]) . (rf[e,x]))) . (fr[e,x])] // S44
    irreflexive[s . (Fsb[e,x]) . (fr[e,x])]                          // S5
    irreflexive[s . (fr[e,x]) . (sbF[e,x])]                          // S6
    irreflexive[s . (Fsb[e,x]) . (fr[e,x]). (sbF[e,x])]              // S7  
  }
}

pred consistent[e:E, x : Exec_C] {
  some s : E -> E {
    wf_s[e,x,s]
    consistentS[e,x,s]
  }
  HbCom[e,x]
  NaRf[e,x]
}
