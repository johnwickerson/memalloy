/* A version of C11 with strong release-acquire semantics.
   (See Lahav et al., POPL 2016) */

module c11_sra[E]
open c11_base[E]

pred SRA[e:E, x:Exec_C] {
  is_acyclic[co[e,x] + sb[e,x] + sw[e,x]]
}

pred consistent[e:E, x : Exec_C] {
  HbCom[e,x]
  SRA[e,x]
  NaRf[e,x]
  Ssimp[e,x]
}		      		      

