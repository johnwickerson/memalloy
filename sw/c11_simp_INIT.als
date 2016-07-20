/* Manually generated from c11_simp.cat */

module c11_simp_INIT[E]
open c11_base_INIT[E]

pred Ssimp[x : Exec_C] {
  let scp = ((rc[Fsb[x]]) . (x.co + fr[x] + (hb[x])) .
    (rc[sbF[x]])) & (x.sc -> x.sc) - iden |
  is_acyclic[scp]
}

pred consistent[x : Exec_C] {
  HbCom[x]
  NaRf[x]
  Ssimp[x]
}		      		      

pred racefree[x : Exec_C] {
  Dr[x]
  Ur[x]
}

