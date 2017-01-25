/* Manually generated from c11_simp.cat */

module c11_simp[E]
open c11_base[E]

pred consistent[e:E, x : Exec_C] {
  HbCom[e,x]
  NaRf[e,x]
  Ssimp[e,x]
}

