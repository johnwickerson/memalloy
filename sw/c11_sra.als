module c11_sra[E]
open c11_base[E]

pred Ssimp[x : Exec_C] {
  let scp = ((rc[Fsb[x]]) . (x.co + fr[x] + (hb[x])) .
    (rc[sbF[x]])) & (x.sc -> x.sc) - iden |
  is_acyclic[scp]
}

pred SRA[x:Exec_C] {
  is_acyclic[x.co + x.sb + x.sw]
}

pred consistent[x : Exec_C] {
  HbCom[x]
  SRA[x]
  NaRf[x]
  Ssimp[x]
}		      		      

pred racefree[x : Exec_C] {
  Dr[x]
  Ur[x]
}

