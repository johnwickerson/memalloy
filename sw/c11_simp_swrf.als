

module c11_simp_swrf[E]
open c11_base[E]

pred Ssimp[x : Exec_C] {
  let scp = ((rc[Fsb[x]]) . (x.co + fr[x] + (hb[x])) .
    (rc[sbF[x]])) & (x.sc -> x.sc) - iden |
  is_acyclic[scp]
}

pred consistent[x : Exec_C] {
  is_acyclic[sw[x] + x.rf]
  HbCom[x]
  NaRf[x]
  Ssimp[x]
}		      		      

pred racefree[x : Exec_C] {
  Dr[x]
  Ur[x]
}

