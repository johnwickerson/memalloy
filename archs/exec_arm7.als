module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {
  isb : E -> E, // control fence
  dmbst, dmbld, dmb : E -> E, // store, load, full fences
}{
  is_fence_rel[dmbst, sb]
  is_fence_rel[dmbld, sb]
  is_fence_rel[dmb, sb]
  is_fence_rel[isb, sb]  
}

fun isb[e:E, X:Exec_Arm7] : E->E { X.isb - (univ -> e) - (e -> univ) }
fun dmbst[e:E, X:Exec_Arm7] : E->E { X.dmbst - (univ -> e) - (e -> univ) }
fun dmbld[e:E, X:Exec_Arm7] : E->E { X.dmbld - (univ -> e) - (e -> univ) }
fun dmb[e:E, X:Exec_Arm7] : E->E { X.dmb - (univ -> e) - (e -> univ) }

// Synonyms:
fun ISB[e:E, X:Exec_Arm7] : E->E { isb[e,X] }
fun DMB[e:E, X:Exec_Arm7] : E->E { dmb[e,X] }
fun DSB[e:E, X:Exec_Arm7] : E->E { dmb[e,X] } // dsb = dmb
fun DMBSY[e:E, X:Exec_Arm7] : E->E { dmb[e,X] }
fun DMBST[e:E, X:Exec_Arm7] : E->E { dmbst[e,X] }
fun DMBLD[e:E, X:Exec_Arm7] : E->E { dmbld[e,X] }
fun DSBST[e:E, X:Exec_Arm7] : E->E { dmbst[e,X] } // dsb = dmb
