module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {
  dmbst, dmbld, dmb, isb : E->E
}{
  is_fence_rel[dmbst, sb]
  is_fence_rel[dmbld, sb]
  is_fence_rel[dmb, sb]
  is_fence_rel[isb, sb]

  dmb in dmbst
  dmb in dmbld 
}

fun isb[e:E, X:Exec_Arm7] : E->E { isb - (univ -> e) - (e -> univ) }
fun dmbst[e:E, X:Exec_Arm7] : E->E { dmbst - (univ -> e) - (e -> univ) }
fun dmbld[e:E, X:Exec_Arm7] : E->E { dmbld - (univ -> e) - (e -> univ) }
fun dmb[e:E, X:Exec_Arm7] : E->E { dmb - (univ -> e) - (e -> univ) }
