module exec_x86[E]
open exec_H[E]

sig Exec_X86 extends Exec_H {
  LOCKED : set E, // atomic events
  mfence : E -> E, // memory fence
}{
  // only RMWs can be locked
  LOCKED in univ.atom + atom.univ

  // the atom relation only relates locked instructions
  atom in (LOCKED -> LOCKED)

  is_fence_rel[mfence, sb] 
}

one sig rm_mfence extends PTag {}

fun locked[e:PTag->E, X:Exec_X86] : set E { X.LOCKED - e[rm_EV] }
fun mfence[e:PTag->E, X:Exec_X86] : E->E {
  (univ - e[rm_EV] - e[rm_mfence]) <: X.mfence :> (univ - e[rm_EV]) }
