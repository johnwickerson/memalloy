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

fun locked[e:E, X:Exec_X86] : set E { X.LOCKED - e }
fun mfence[e:E, X:Exec_X86] : E->E { X.mfence - (univ -> e) - (e -> univ) }
