/* A version of C11 with sw+rf cycles forbidden.
  (See Nienhuis et al., OOPSLA 2016) */

module c11_simp_swrf[E]
open c11_base[E]

pred SwRf[e:E, x:Exec_C] {
  is_acyclic[sw[e,x] + rf[e,x]]
}

pred consistent[e:E, x : Exec_C] {
  SwRf[e,x]
  HbCom[e,x]
  NaRf[e,x]
  Ssimp[e,x]
}		      		      



