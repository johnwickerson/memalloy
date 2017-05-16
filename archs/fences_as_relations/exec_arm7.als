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

one sig rm_isb extends PTag {}
one sig rm_dmbst extends PTag {}
one sig rm_dmbld extends PTag {}
one sig rm_dmb extends PTag {}

fun isb[e:PTag->E, X:Exec_Arm7] : E->E {
  (univ - e[rm_EV] - e[rm_isb]) <: X.isb :> (univ - e[rm_EV]) }
fun dmbst[e:PTag->E, X:Exec_Arm7] : E->E {
  (univ - e[rm_EV] - e[rm_dmbst]) <: X.dmbst :> (univ - e[rm_EV]) }
fun dmbld[e:PTag->E, X:Exec_Arm7] : E->E {
  (univ - e[rm_EV] - e[rm_dmbld]) <: X.dmbld :> (univ - e[rm_EV]) }
fun dmb[e:PTag->E, X:Exec_Arm7] : E->E {
  (univ - e[rm_EV] - e[rm_dmb]) <: X.dmb :> (univ - e[rm_EV]) }
