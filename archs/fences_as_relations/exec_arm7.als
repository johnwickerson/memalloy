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

  // control dependencies only come out of reads
  cd in (R -> EV)
}

one sig rm_isb extends PTag {}
one sig rm_dmbst extends PTag {}
one sig rm_dmbld extends PTag {}
one sig rm_dmb extends PTag {}

fun isb[e:PTag->E, X:Exec_Arm7] : E->E {
  mk_fence_rel[e, rm_isb, X.isb, X.sb] }

fun dmbst[e:PTag->E, X:Exec_Arm7] : E->E {
  mk_fence_rel[e, rm_dmbst, X.dmbst, X.sb] }
fun dmbld[e:PTag->E, X:Exec_Arm7] : E->E {
  mk_fence_rel[e, rm_dmbld, X.dmbld, X.sb] }
fun dmb[e:PTag->E, X:Exec_Arm7] : E->E {
  mk_fence_rel[e, rm_dmb + rm_dmbld + rm_dmbst, X.dmb, X.sb] }
