/* Manually generated from c11_simp.cat */

module c11_simp[E]
open c11_base[E]

pred consistent[x : Exec_C] {
  HbCom[x]
  NaRf[x]
  Ssimp[x]
}

