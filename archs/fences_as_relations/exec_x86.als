module exec_x86[E]
open exec_H[E]

sig Exec_X86 extends Exec_H {
  mfence : E -> E, // memory fence
}{
  is_fence_rel[mfence, sb] 
}

one sig rm_mfence extends PTag {}

fun mfence[e:PTag->E, X:Exec_X86] : E->E {
  (univ - e[rm_EV] - e[rm_mfence]) <: X.mfence :> (univ - e[rm_EV]) }
