/* A version of C11 with strong release-acquire semantics.
   (See Lahav et al., POPL 2016) */

module c11_sra[E]
open c11_base[E]

pred SRA[x:Exec_C] {
  is_acyclic[x.co + x.sb + x.sw]
}

pred consistent[x : Exec_C] {
  HbCom[x]
  SRA[x]
  NaRf[x]
  Ssimp[x]
}		      		      

