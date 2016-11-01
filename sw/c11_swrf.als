/* A version of C11 with sw+rf cycles forbidden.
  (See Nienhuis et al., OOPSLA 2016) */

module c11_simp_swrf[E]
open c11_base[E]

pred SwRf[x:Exec_C] {
  is_acyclic[sw[x] + x.rf]
}

pred consistent[x : Exec_C] {
  SwRf[x]
  HbCom[x]
  NaRf[x]
  Ssimp[x]
}		      		      



