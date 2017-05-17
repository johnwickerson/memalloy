module exec_x86[E]
open exec_H[E]

sig Exec_X86 extends Exec_H {
  LOCKED : set E, // atomic events
  MFENCE : set E // memory fence
}{

  MFENCE in F
    
  // only RMWs can be locked
  LOCKED in univ.atom + atom.univ

  // the atom relation only relates locked instructions
  atom in (LOCKED -> LOCKED)
}

one sig rm_LOCKED extends PTag {}
one sig rm_MFENCE extends PTag {}

fun LOCKED[e:PTag->E, X:Exec_X86] : set E {
  X.LOCKED - e[rm_EV] - e[rm_LOCKED] }
fun MFENCE[e:PTag->E, X:Exec_X86] : set E {
  X.MFENCE - e[rm_EV] - e[rm_MFENCE] }

fun mfence[e:PTag->E, X:Exec_X86] : E->E { addsb[e,X,MFENCE[e,X]] }
