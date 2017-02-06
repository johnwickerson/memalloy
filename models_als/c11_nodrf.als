// Exec_C

/* A weaker version of c11_orig that can be shown to violate the
   "DRF-for-SC" guarantee.
*/

module c11_orig_nodrf[E]
open c11_base[E]

pred consistentS[e:E, x : Exec_C, s : E -> E] {
  irreflexive[s . (hb[e,x])]                                   // S1
  irreflexive[s . (rc[Fsb[e,x]]) . (co[e,x]) . (rc[sbF[e,x]])] // S2
  irreflexive[s . ~(rf[e,x]) . (stor[sc[e,x]]) . (co[e,x])]    // S3
  irreflexive[s . (Fsb[e,x]) . (fr[e,x])]                      // S5
  irreflexive[s . (fr[e,x]) . (sbF[e,x])]                      // S6
  irreflexive[s . (Fsb[e,x]) . (fr[e,x]). (sbF[e,x])]          // S7  
}

pred consistent[e:E, x : Exec_C] {
  some s : E -> E {
    wf_s[e,x,s]
    consistentS[e,x,s]
  }
  HbCom[e,x]
  NaRf[e,x]
}
